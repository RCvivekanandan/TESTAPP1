00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELXPMCCP
00003  PROGRAM-ID.         ELXPMCCP.                                       LV004
00004                                                                   ELXPMCCP
00005  AUTHOR.             ANNE KEFFER-KING.                            ELXPMCCP
00006                      REWRITTEN BY ROBERT OEHMAN.                  ELXPMCCP
00007                                                                   ELXPMCCP
00008                                                                   ELXPMCCP
00009  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCCP
00010                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCCP
00011                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCCP
00012                      233 N. MICHIGAN AVE                          ELXPMCCP
00013                      CHICAGO, ILLINOIS 60601                      ELXPMCCP
00014                                                                   ELXPMCCP
00015  DATE-WRITTEN.       14-JUL-1992.                                 ELXPMCCP
00016                                                                   ELXPMCCP
00017  DATE-COMPILED.                                                   ELXPMCCP
00018                                                                   ELXPMCCP
00019  SECURITY.           COPYRIGHT 1992,                              ELXPMCCP
00020                      HEALTH CARE SERVICE CORPORATION              ELXPMCCP
00021      SKIP3                                                        ELXPMCCP
00022  ENVIRONMENT DIVISION.                                            ELXPMCCP
00023                                                                   ELXPMCCP
00024  CONFIGURATION SECTION.                                           ELXPMCCP
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELXPMCCP
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELXPMCCP
00027      EJECT                                                        ELXPMCCP
00028 ******************************************************************ELXPMCCP
00029 *AKK 12/06/05 REGEN FOR TEST                                     *ELXPMCCP
00030 *                                                               * ELXPMCCP
00031 *   PROGRAM:  ELXPMCCP                                           *ELXPMCCP
00032 *   DATE:     07/16/92                                           *ELXPMCCP
00033 *   AUTHOR:   ANNE KEFFER-KING                                   *ELXPMCCP
00034 *   FUNCTION:                                                    *ELXPMCCP
00035 *       THIS PROGRAM WILL DETERMINE WHICH OF THE REQUESTED       *ELXPMCCP
00036 *       CCP S ARE COVERED FOR NA/ES PROJECT.  IT WILL ONLY TAG   *ELXPMCCP
00037 *       YES/NO FOR COVERAGE IT WILL NOT DETERMINE OR SEARCH FOR  *ELXPMCCP
00038 *       REQUESTED ACCUMS.  THAT FUNCTION WILL BE PERFORMED BY    *ELXPMCCP
00039 *       ELXPMCAC.                                                *ELXPMCCP
00040 *       FOLLOWING IS A LIST OF INTERNAL ERROR CODES USED BY      *ELXPMCCP
00041 *       THIS PROGRAM:                                            *ELXPMCCP
00042 *       1000  UNABLE TO ESTABLISH ENVIRONMENT                    *ELXPMCCP
00043 *       1001  UNABLE TO FIND GROUP SPECIFIC RECORD               *ELXPMCCP
00044 *       1002  #GPPO NOT CODED BUT REQUIRED                       *ELXPMCCP
00045 *       1003  NO FILE KEY WORK AREA FOUND -- UNABLE TO           *ELXPMCCP
00046 *             ADDRESS                                            *ELXPMCCP
00047 *       1004  NO INTERMEDIATE DATA AREA FOUND                    *ELXPMCCP
00048 *       1005  NO PMCI COMMON AREA FOUND                          *ELXPMCCP
00049 *             ADDRESS                                            *ELXPMCCP
00050 *       1006  INVALID PPO DATA                                   *ELXPMCCP
00051 *       1007  INVALID GPPO SWITCH SETTING                        *ELXPMCCP
00052 *       1008  UNDEFINED GCTABULAR                                *ELXPMCCP
00053 *       1009  GCTABULAR NOT FOUND                                *ELXPMCCP
00054 *       1010  CRITICAL I/O ERROR                                 *ELXPMCCP
00055 *       1011  ADDRESSIBIILTY TO GCCP FAILED                      *ELXPMCCP
00056 *       1012  INVALID RPO DATA                                   *ELXPMCCP
00057 *       1013  INVALID GRPO SWITCH SETTING                        *ELXPMCCP
00058 *       1014  INVALID BAE DATA                                   *ELXPMCCP
00059 *       1015  INVALID GBAE SWITCH SETTING                        *ELXPMCCP
00060 ******************************************************************ELXPMCCP
00061 *                      MAINTENANCE HISTORY                       *ELXPMCCP
00062 *  MOD     DATE     BY  DRPT                ACTION               *ELXPMCCP
00063 * ----- ----------- --- ----- ---------------------------------- *ELXPMCCP
00064 * 01.00 14-JUL-1992 AKK       CREATED                            *ELXPMCCP
00065 *                                                                *ELXPMCCP
00066 * 01.01 26-AUG-1992 AKK       ADDED CODE TO DETERMINE WHEN THE   *ELXPMCCP
00067 *                             CCP PROGRAM APPLIES TO THE TYPE    *ELXPMCCP
00068 *                             OF PROVIDER I.E. INST OR PROF      *ELXPMCCP
00069 *                                                                *ELXPMCCP
00070 * 01.02 12-OCT-1992 AKK       CHANGED PPO CODE TO CORRECT CHECK  *ELXPMCCP
00071 *                             FOR INST AND PROF CALL SITUATIONS  *ELXPMCCP
00072 *                                                                *ELXPMCCP
00073 * 01.03 22-DEC-1992 AKK       CORRECTED ASRA FOUND DURING TEST-  *ELXPMCCP
00074 *                             ING OF OTHER NA/ES MODULES.        *ELXPMCCP
00075 *                                                                *ELXPMCCP
00076 * 02.00 27-MAY-1992 RGO       ISSR 13071. SUPPORT SUPPLEMENTAL   *ELXPMCCP
00077 *                             MAJOR MEDICAL BENIFITS PROJECT.    *ELXPMCCP
00078 *                             FOR EACH FIELD/BENIFIT/PROGRAM     *ELXPMCCP
00079 *                             CHECKED BY THIS PROGRAM:           *ELXPMCCP
00080 *                             1) SEPARATE THE INSTITUTIONAL '88' *ELXPMCCP
00081 *                                PARTICIPATE INDICATOR FIELD AND *ELXPMCCP
00082 *                                PROFESSIONAL '88' PARTICIPATE   *ELXPMCCP
00083 *                                INDICATOR INTO 3 DIVISONS:      *ELXPMCCP
00084 *                                                                *ELXPMCCP
00085 *                                A) COVERED BY BASIC             *ELXPMCCP
00086 *                                B) COVERED BY MAJOR MEDICAL     *ELXPMCCP
00087 *                                C) COVERED BY BASIC & MAJOR MED.*ELXPMCCP
00088 *                                                                *ELXPMCCP
00089 *                             2) FOR EACH FIELD, A NEW FIELD     *ELXPMCCP
00090 *                                HAS BEEN ADDED TO THE COMM AREA.*ELXPMCCP
00091 *                                THIS TELLS WHERE THE COVERAGE IS*ELXPMCCP
00092 *                                DERIVED FROM. THE VALUES ARE:   *ELXPMCCP
00093 *                                 1) ' ' (BLANK) - BASIC         *ELXPMCCP
00094 *                                 2) '*' - MAJOR MEDICAL         *ELXPMCCP
00095 *                                 3) '+' - BOTH BASIC & MAJ MED  *ELXPMCCP
00096 *                                                                *ELXPMCCP
00097 *                             3) EACH FIELD PARAGRAPH HAS BEEN   *ELXPMCCP
00098 *                                RE-STRUCTURED TO FIRST CHECK    *ELXPMCCP
00099 *                                WHO IS MAKING THE REQUEST, AND  *ELXPMCCP
00100 *                                THEN DETERMINE COVERAGE.        *ELXPMCCP
00101 * 02.01 12-JUL-1992 AKK       ISSR 13071.  ADDED SPECIAL LOGIC   *ELXPMCCP
00102 *                             TO MSA PROCESSING.  WE WILL NOW    *ELXPMCCP
00103 *                             THE GCCP TABULAR AND CHECK THE     *ELXPMCCP
00104 *                             BC, BS OR MM INDICATOR. THE RESULT *ELXPMCCP
00105 *                             WILL DETERMINE ANY FURTHER PROCESS-*ELXPMCCP
00106 *                             ING.  A CALL OR A NO WILL PRECLUDE *ELXPMCCP
00107 *                             ANY FURTHER PROCESSING IN ELXPMCAP.*ELXPMCCP
00108 *                             A YES WILL MEAN PROCESS AS USUAL.  *ELXPMCCP
00109 * 02.02 16-AUG-1993 BAK       CORRECT ERROR FOR MSA PROCESSING   *ELXPMCCP
00110 *                             IF MSA PARTICIPATION INDICATOR = 00 ELXPMCCP
00111 *                             DO NOT READ FOR GCCP TABULAR.       ELXPMCCP
00112 * 02.03 06-OCT-1993 AKK       ADDING BS IND 27 AND 28 -- BS IND  *ELXPMCCP
00113 *                             0H, 0I AND 0J.                      ELXPMCCP
00114 * 03.00 24-SEP-1993 BAK       ADD SUPPORT FOR PHASE 2 FOR ISSR   *ELXPMCCP
00115 *                             13071 FOR RPO AND MCN-P SUPPORT.    ELXPMCCP
00116 * 03.01 11-NOV-1993 BAK       CORRECT PROCESSING FOR RPO IN PPO  *ELXPMCCP
00117 *                                                                 ELXPMCCP
00118 * 04.00 25-APR-1994 RGO       UPDATE THE HOLD-BSC-GCCP-IND AND    ELXPMCCP
00119 *                                        HOLD-MM-GCCP-IND FIELDS  ELXPMCCP
00120 *                             TO REFLECT NEW CODES ADDED TO THE   ELXPMCCP
00121 *                             CODES MANUAL. CHANGED THE DEFAULT   ELXPMCCP
00122 *                             PROCESSING IN SUB-PROC'S 5180- TO   ELXPMCCP
00123 *                             5187- SO THAT ALL NON-ZERO VALUES   ELXPMCCP
00124 *                             RESULT IN A CALL.                   ELXPMCCP
00125 *                             ANY TIME #GCCP CODES # 139, 143 OR  ELXPMCCP
00126 *                             149 CHANGE, THIS PROGRAM SHOULD BE  ELXPMCCP
00127 *                             UPDATED.                            ELXPMCCP
00128 * 3/6/95  RGO   CPO PROJECT.  ADD CODE TO USE THE CPO INDICATOR.  ELXPMCCP
00129 *                             PMCCOMM WAS ALSO UPDATED.           ELXPMCCP
00130 *                                                                 ELXPMCCP
00131 * 4/2/96  RGO   CBL PROJECT.  ADD CODE TO USE THE CBL INDICATOR.  ELXPMCCP
00132 *                             PMCCOMM WAS ALSO UPDATED.           ELXPMCCP
00133 *                                                                 ELXPMCCP
00134 * 4/1/03  AKK   ENDEVOR       MORE CHANGES DUES TO ENDEVOR.       ELXPMCCP
00135 *                                                                 ELXPMCCP
00136 * 02.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCCP
00137 *                                                                *ELXPMCCP
00138 * 02.01 09-JAN-2004 AKK INTERTEST S0C7                           *ELXPMCCP
00139 *                                                                 ELXPMCCP
00140 * 02.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELXPMCCP
00141 *                                                                *ELXPMCCP
00142 *                                                                *ELXPMCCP
00143 ******************************************************************ELXPMCCP
00144                                                                   ELXPMCCP
00145 /                                                                 ELXPMCCP
00146  DATA DIVISION.                                                   ELXPMCCP
00147  WORKING-STORAGE SECTION.                                         ELXPMCCP
00148  01  WS-HDR                      PIC      X(42) VALUE             ELXPMCCP
00149      '***ELXPMCCP WORKING STORAGE BEGINS HERE***'.                ELXPMCCP
00150  01  WS-MISC.                                                     ELXPMCCP
00151      05  WS-TABULAR              PIC      X(06) VALUE SPACES.     ELXPMCCP
00152      05  WS-GPPO                 PIC      X(06) VALUE '#GPPO '.   ELXPMCCP
00153      05  WS-GRPO                 PIC      X(06) VALUE '#GRPO '.   ELXPMCCP
00154      05  WS-GCCP                 PIC      X(06) VALUE '#GCCP '.   ELXPMCCP
00155      05  WS-GCCP-MAX-IDX         USAGE IS INDEX.                  ELXPMCCP
00156      05  HOLD-GSS-IDX            USAGE IS INDEX.                  ELXPMCCP
00157  01  WS-SWITCHES.                                                 ELXPMCCP
00158      05 WS-TERMINAL-SWITCH       PIC      X(01).                  ELXPMCCP
00159         88 SW-TRMNL-ERR          VALUE 'Y'.                       ELXPMCCP
00160         88 SW-NO-TRMNL-ERR       VALUE 'N'.                       ELXPMCCP
00161      05 WS-TAB-SWITCH            PIC      X(01).                  ELXPMCCP
00162         88 WS-NO-TAB             VALUE 'N'.                       ELXPMCCP
00163         88 WS-NO-MATCH-TAB       VALUE 'M'.                       ELXPMCCP
00164         88 WS-TAB-INCLUDE        VALUE 'I'.                       ELXPMCCP
00165         88 WS-TAB-EXCLUDE        VALUE 'E'.                       ELXPMCCP
00166      05 WS-TAB-FOUND-SWITCH      PIC      X(01).                  ELXPMCCP
00167         88 WS-TAB-FOUND          VALUE 'F'.                       ELXPMCCP
00168         88 WS-NO-TAB-FOUND       VALUE 'N'.                       ELXPMCCP
00169      05 WS-GCCP-TABULAR-SWITCH   PIC      X(01) VALUE 'N'.        ELXPMCCP
00170         88 WS-GCCP-TAB-FOUND     VALUE 'Y'.                       ELXPMCCP
00171         88 WS-GCCP-TAB-NOT-FND   VALUE 'N'.                       ELXPMCCP
00172      05 WS-GCCP-FOUND-SW         PIC      X(01) VALUE 'N'.        ELXPMCCP
00173         88 WS-GCCP-FOUND         VALUE 'Y'.                       ELXPMCCP
00174         88 WS-GCCP-NOT-FOUND     VALUE 'N'.                       ELXPMCCP
00175 /                                                                 ELXPMCCP
00176 *THE INDICATORS BELOW ARE PULLED FROM THE MSA INDICATORS ON THE GCELXPMCCP
00177 *RECORD.                                                          ELXPMCCP
00178 * LAST UPDATED: 4/25/94 TO INCLUDE CODES '0Z', '29 THRU 36'.      ELXPMCCP
00179 *     AND A ZERO VALUE.  ZEROS ALWAYS MEAN 'NOT APPLICABLE'. RGO  ELXPMCCP
00180 ***************************************************************** ELXPMCCP
00181                                                                   ELXPMCCP
00182  01 WS-MSA-GCCP-INDICATORS.                                       ELXPMCCP
00183     05 WS-HOLD-BSC-GCCP-IND             PIC X(02).                ELXPMCCP
00184      88 WS-BC-PRCSS-IP-MSA VALUE '0B' '0C' '0E' '0F'              ELXPMCCP
00185                                  '0G' '0H' '0I' '0J'              ELXPMCCP
00186                                  '0M' '0P' '0R' '0S'              ELXPMCCP
00187                                  '0T' '0U' '0Y' '02'              ELXPMCCP
00188                                  '03' '04' '06' '07'              ELXPMCCP
00189                                  '08' '09' '10' '12'              ELXPMCCP
00190                                  '15' '16' '17' '18'              ELXPMCCP
00191                                  '19' '21' '22' '23'              ELXPMCCP
00192                                  '25' '26' '27' '33'.             ELXPMCCP
00193      88 WS-BC-CALL-IP-MSA  VALUE '0A' '0D' '0K' '0L'              ELXPMCCP
00194                                  '0V' '0W' '0X' '01'              ELXPMCCP
00195                                  '05' '11' '13' '14'              ELXPMCCP
00196                                  '20' '24' '28'.                  ELXPMCCP
00197      88 WS-BC-PRCSS-OP-MSA VALUE '0A' '0E' '0F' '0G'              ELXPMCCP
00198                                  '0H' '0I' '0J' '0M'              ELXPMCCP
00199                                  '0P' '0R' '0Y' '02'              ELXPMCCP
00200                                  '03' '04' '06' '12'              ELXPMCCP
00201                                  '15' '17' '18' '19'              ELXPMCCP
00202                                  '22' '23' '27' '33'.             ELXPMCCP
00203      88 WS-BC-CALL-OP-MSA   VALUE '0D' '0K' '0L' '0V'             ELXPMCCP
00204                                   '01' '05' '11' '13'             ELXPMCCP
00205                                   '20' '28'.                      ELXPMCCP
00206      88 WS-BS-PRCSS-IP-MSA VALUE '0C' '0D' '0E' '0F' '0G'         ELXPMCCP
00207                                  '01' '02' '03' '05'.             ELXPMCCP
00208      88 WS-BS-PRCSS-OP-MSA VALUE '0A' '0D' '0E' '0F' '0G'         ELXPMCCP
00209                                  '0H' '01' '02' '04' '05'         ELXPMCCP
00210                                  '06' '08' '09'.                  ELXPMCCP
00211      88 WS-BS-CALL-IP-MSA  VALUE '0B' '0I' '0J'.                  ELXPMCCP
00212      88 WS-BS-CALL-OP-MSA  VALUE '0B' '07'.                       ELXPMCCP
00213      88 WS-BSC-NOT-APPL        VALUE '00' '  '.                   ELXPMCCP
00214     05 WS-HOLD-MM-GCCP-IND              PIC X(02).                ELXPMCCP
00215      88 WS-MM-PRCSS-IP-MSA VALUE '02' '03' '04' '05' '06'.        ELXPMCCP
00216      88 WS-MM-CALL-MSA     VALUE '01'.                            ELXPMCCP
00217      88 WS-MM-PRCSS-OP-MSA VALUE '02' '05' '06'.                  ELXPMCCP
00218      88 WS-MM-NOT-APPL        VALUE '00' '  '.                    ELXPMCCP
00219                                                                   ELXPMCCP
00220 ******************************************************************ELXPMCCP
00221 * FOR MSA MM DOES NOT HAVE ANY SPECIFIC BREAKDOWNS FOR IP/OP     *ELXPMCCP
00222 ******************************************************************ELXPMCCP
00223 * IN EACH OF THE FOLLOWING FIELDS, THE PARTICIPATION INDICATOR IS ELXPMCCP
00224 *       PLACED IN AN '88' LEVEL GROUP WHICH DESCRIBES             ELXPMCCP
00225 *       1) WHO THE COVERAGE IS VALID FOR - AN INSTITUTION OR A    ELXPMCCP
00226 *          A PROFESSIONAL. (HOSPITAL VS. A DOCTOR).               ELXPMCCP
00227 *       2) UNDER WHAT PLAN IS THE COVERAGE UNDER - BASIC,         ELXPMCCP
00228 *          MAJOR MEDICAL, OR BOTH BASIC AND MAJOR MEDICAL.        ELXPMCCP
00229 *                                                                 ELXPMCCP
00230 * MANY PARTICIPATION INDICATORS ARE COVERED UNDER BOTH INSTITUTIONELXPMCCP
00231 * AND PROFESSIONAL.                                      RGO-5/27/ELXPMCCP
00232 *                                                                 ELXPMCCP
00233 ******************************************************************ELXPMCCP
00234 ******************************************************************ELXPMCCP
00235 *   ATCP              ADDITIONAL TRANSPLANT COVERAGE PROGRAM      ELXPMCCP
00236 ******************************************************************ELXPMCCP
00237  01  WS-ATCP-PART-IND            PIC X(02).                       ELXPMCCP
00238      88  WS-ATCP-INST-BSC        VALUE '01' '04' '0C' '0F'.       ELXPMCCP
00239      88  WS-ATCP-INST-MM         VALUE '03' '07' '0E'.            ELXPMCCP
00240      88  WS-ATCP-INST-BOTH       VALUE '0G' '05'.                 ELXPMCCP
00241      88  WS-ATCP-INST-NO         VALUE '00' '0D'.                 ELXPMCCP
00242                                                                   ELXPMCCP
00243      88  WS-ATCP-PROF-BSC        VALUE '02' '04' '0D' '0F'.       ELXPMCCP
00244      88  WS-ATCP-PROF-MM         VALUE '03' '06' '0E'.            ELXPMCCP
00245      88  WS-ATCP-PROF-BOTH       VALUE '05' '07' '0G'.            ELXPMCCP
00246      88  WS-ATCP-PROF-NO         VALUE '00' '01' '0C'.            ELXPMCCP
00247                                                                   ELXPMCCP
00248      88  WS-ATCP-CALL            VALUE '08'.                      ELXPMCCP
00249                                                                   ELXPMCCP
00250 ******************************************************************ELXPMCCP
00251 * VALUES COMMON TO HOSPICE, MASOP, MSA AND WEEKEND                ELXPMCCP
00252 *                                                                 ELXPMCCP
00253 ******************************************************************ELXPMCCP
00254  01  WS-COMMON-PART-IND         PIC X(02).                        ELXPMCCP
00255      88  WS-COMMON-INST-BSC     VALUE '01' '04'.                  ELXPMCCP
00256      88  WS-COMMON-INST-MM      VALUE '03' '07'.                  ELXPMCCP
00257      88  WS-COMMON-INST-BOTH    VALUE '05' '06'.                  ELXPMCCP
00258      88  WS-COMMON-INST-NO      VALUE '00' '02'.                  ELXPMCCP
00259                                                                   ELXPMCCP
00260      88  WS-COMMON-PROF-BSC     VALUE '02' '04'.                  ELXPMCCP
00261      88  WS-COMMON-PROF-MM      VALUE '03' '06'.                  ELXPMCCP
00262      88  WS-COMMON-PROF-BOTH    VALUE '05' '07'.                  ELXPMCCP
00263      88  WS-COMMON-PROF-NO      VALUE '00' '01'.                  ELXPMCCP
00264                                                                   ELXPMCCP
00265      88  WS-COMMON-CALL         VALUE '08'.                       ELXPMCCP
00266                                                                   ELXPMCCP
00267 ******************************************************************ELXPMCCP
00268 * MOPS. MANDATORY OUTPATIENT SURGERY                              ELXPMCCP
00269 *       THIS IS THE SAME AS COMMON BUT WITH THE ADDITION OF '09'. ELXPMCCP
00270 ******************************************************************ELXPMCCP
00271  01  WS-MOPS-PART-IND           PIC X(02).                        ELXPMCCP
00272      88  WS-MOPS-INST-BSC       VALUE '01' '04' '09'.             ELXPMCCP
00273      88  WS-MOPS-INST-MM        VALUE '03' '07'.                  ELXPMCCP
00274      88  WS-MOPS-INST-BOTH      VALUE '05' '06'.                  ELXPMCCP
00275      88  WS-MOPS-INST-NO        VALUE '00' '02'.                  ELXPMCCP
00276                                                                   ELXPMCCP
00277      88  WS-MOPS-PROF-BSC       VALUE '02' '04' '09'.             ELXPMCCP
00278      88  WS-MOPS-PROF-MM        VALUE '03' '06'.                  ELXPMCCP
00279      88  WS-MOPS-PROF-BOTH      VALUE '05' '07'.                  ELXPMCCP
00280      88  WS-MOPS-PROF-NO        VALUE '00' '01'.                  ELXPMCCP
00281                                                                   ELXPMCCP
00282      88  WS-MOPS-CALL           VALUE '08'.                       ELXPMCCP
00283                                                                   ELXPMCCP
00284                                                                   ELXPMCCP
00285 ******************************************************************ELXPMCCP
00286 * MEDNEC.  MEDICAL NECESSITY                                      ELXPMCCP
00287 ******************************************************************ELXPMCCP
00288  01  WS-MEDNEC-PART-IND          PIC X(02).                       ELXPMCCP
00289      88  WS-MEDNEC-INST-BSC      VALUE '0A' '0C' '0F' '01'        ELXPMCCP
00290                                        '02' '03' '06' '08'.       ELXPMCCP
00291      88  WS-MEDNEC-INST-MM       VALUE '0E' '07'.                 ELXPMCCP
00292      88  WS-MEDNEC-INST-BOTH     VALUE '0G' '04' '05' '09'.       ELXPMCCP
00293      88  WS-MEDNEC-INST-NO       VALUE '00' '0B' '0D'.            ELXPMCCP
00294                                                                   ELXPMCCP
00295      88  WS-MEDNEC-PROF-BSC      VALUE '0B' '0D' '0F' '02'        ELXPMCCP
00296                                        '03' '08'.                 ELXPMCCP
00297      88  WS-MEDNEC-PROF-MM       VALUE '0A' '0E' '05' '07'.       ELXPMCCP
00298      88  WS-MEDNEC-PROF-BOTH     VALUE '0G' '04' '09'.            ELXPMCCP
00299      88  WS-MEDNEC-PROF-NO       VALUE '00' '0C' '01' '06'.       ELXPMCCP
00300                                                                   ELXPMCCP
00301                                                                   ELXPMCCP
00302 ******************************************************************ELXPMCCP
00303 * PAR. PRE-ADMISSION REVIEW.                                      ELXPMCCP
00304 ******************************************************************ELXPMCCP
00305  01  WS-PAR-PART-IND             PIC X(02).                       ELXPMCCP
00306      88  WS-PAR-INST-BSC         VALUE '0A' '01' '04' '09'.       ELXPMCCP
00307      88  WS-PAR-INST-MM          VALUE '03' '07'.                 ELXPMCCP
00308      88  WS-PAR-INST-BOTH        VALUE '05' '06'.                 ELXPMCCP
00309      88  WS-PAR-INST-NO          VALUE '00' '02'.                 ELXPMCCP
00310                                                                   ELXPMCCP
00311      88  WS-PAR-PROF-BSC         VALUE '0A' '02' '04'.            ELXPMCCP
00312      88  WS-PAR-PROF-MM          VALUE '03' '06'.                 ELXPMCCP
00313      88  WS-PAR-PROF-BOTH        VALUE '05' '07'.                 ELXPMCCP
00314      88  WS-PAR-PROF-NO          VALUE '00' '01' '09'.            ELXPMCCP
00315                                                                   ELXPMCCP
00316      88  WS-PAR-CALL             VALUE '08'.                      ELXPMCCP
00317                                                                   ELXPMCCP
00318 ******************************************************************ELXPMCCP
00319 * PPO. PARTICIPATION PROVIDER OPTION.                             ELXPMCCP
00320 ******************************************************************ELXPMCCP
00321  01  WS-PPO-PART-IND             PIC X(02).                       ELXPMCCP
00322                                                                   ELXPMCCP
00323 *    THE FOLLOWING ARE USED TO DETERMINE IF PPO APPLIES           ELXPMCCP
00324                                                                   ELXPMCCP
00325      88  WS-PPO-INST-BSC         VALUE '0B' '0F' '0G' '0H' '0I'   ELXPMCCP
00326                                        '0J' '0M' '0N' '0P' '01'   ELXPMCCP
00327                                        '04' '08' '11' '12' '13'   ELXPMCCP
00328                                        '14' '15'.                 ELXPMCCP
00329      88  WS-PPO-INST-MM          VALUE '0A' '03'.                 ELXPMCCP
00330      88  WS-PPO-INST-BOTH        VALUE '0C' '0D' '0K' '0L' '05'   ELXPMCCP
00331                                        '06'.                      ELXPMCCP
00332      88  WS-PPO-INST-NO          VALUE '00' '02' '07' '09'.       ELXPMCCP
00333                                                                   ELXPMCCP
00334      88  WS-PPO-PROF-BSC         VALUE '0B' '0F' '0G' '0I' '0M'   ELXPMCCP
00335                                        '0N' '0P' '02' '04' '09'   ELXPMCCP
00336                                        '10' '11' '12' '13' '14'   ELXPMCCP
00337                                        '15'.                      ELXPMCCP
00338      88  WS-PPO-PROF-MM          VALUE '0A' '0K' '03'.            ELXPMCCP
00339      88  WS-PPO-PROF-BOTH        VALUE '0C' '0E' '0L' '05' '07'.  ELXPMCCP
00340      88  WS-PPO-PROF-NO          VALUE '00' '0H' '0J' '01' '06'   ELXPMCCP
00341                                        '08'.                      ELXPMCCP
00342                                                                   ELXPMCCP
00343 *    THE FOLLOWING ARE USED TO DETERMINE IF THE PROVIDER IS       ELXPMCCP
00344 *        IN OR OUT OF NETWORK.                                    ELXPMCCP
00345                                                                   ELXPMCCP
00346      88  WS-PPO-INST-STD         VALUE '01' '03' '04' '05' '06'   ELXPMCCP
00347                                        '13' '14' '15'.            ELXPMCCP
00348      88  WS-PPO-INST-NON-STD     VALUE '0A' '0B' '0C' '0D'        ELXPMCCP
00349                                        '0M' '0N' '0P' '08'        ELXPMCCP
00350                                        '11' '12'.                 ELXPMCCP
00351      88  WS-PPO-INST-CALL        VALUE '0G' '0J' '0K' '0L'.       ELXPMCCP
00352      88  WS-PPO-INST-SPCL        VALUE '0I'.                      ELXPMCCP
00353                                                                   ELXPMCCP
00354      88  WS-PPO-PROF-STD         VALUE '02' '03' '04' '05' '07'   ELXPMCCP
00355                                        '13' '14' '0M'.            ELXPMCCP
00356      88  WS-PPO-PROF-MPP         VALUE '0N'.                      ELXPMCCP
00357      88  WS-PPO-PROF-NON-STD     VALUE '09' '0A' '0B' '0C' '0E'   ELXPMCCP
00358                                        '0P' '10' '11' '12' '15'.  ELXPMCCP
00359      88  WS-PPO-PROF-CALL        VALUE '0G' '0L'.                 ELXPMCCP
00360      88  WS-PPO-PROF-SPCL        VALUE '0I'.                      ELXPMCCP
00361                                                                   ELXPMCCP
00362      88  WS-PPO-AMERITECH        VALUE '0F'.                      ELXPMCCP
00363      88  WS-PPO-ZENITH           VALUE '0H'.                      ELXPMCCP
00364 ******************************************************************ELXPMCCP
00365 * BAE. BLUE ADVANTAGE ENTRPRENEUR                                 ELXPMCCP
00366 ******************************************************************ELXPMCCP
00367  01  WS-BAE-PART-IND             PIC X(02).                       ELXPMCCP
00368                                                                   ELXPMCCP
00369 *    THE FOLLOWING ARE USED TO DETERMINE IF BAE APPLIES           ELXPMCCP
00370                                                                   ELXPMCCP
00371      88  WS-BAE-PROF-YES         VALUE '01' '02'.                 ELXPMCCP
00372      88  WS-BAE-PROF-NO          VALUE '00'.                      ELXPMCCP
00373                                                                   ELXPMCCP
00374 *    THE FOLLOWING ARE USED TO DETERMINE IF THE PROVIDER IS       ELXPMCCP
00375 *        IN OR OUT OF NETWORK.                                    ELXPMCCP
00376                                                                   ELXPMCCP
00377      88  WS-BAE-PROF-STD         VALUE '01'.                      ELXPMCCP
00378      88  WS-BAE-PROF-NON-STD     VALUE '02'.                      ELXPMCCP
00379 ******************************************************************ELXPMCCP
00380 * RPO. PARTICIPATION PROVIDER OPTION.                             ELXPMCCP
00381 ******************************************************************ELXPMCCP
00382  01  WS-RPO-PART-IND             PIC X(02).                       ELXPMCCP
00383                                                                   ELXPMCCP
00384 *    THE FOLLOWING ARE USED TO DETERMINE IF RPO APPLIES           ELXPMCCP
00385                                                                   ELXPMCCP
00386      88  WS-RPO-INST-YES         VALUE '01' '02'.                 ELXPMCCP
00387      88  WS-RPO-INST-NO          VALUE '00'.                      ELXPMCCP
00388                                                                   ELXPMCCP
00389 *    THE FOLLOWING ARE USED TO DETERMINE IF THE PROVIDER IS       ELXPMCCP
00390 *        IN OR OUT OF NETWORK.                                    ELXPMCCP
00391                                                                   ELXPMCCP
00392      88  WS-RPO-INST-STD         VALUE '01'.                      ELXPMCCP
00393      88  WS-RPO-INST-NON-STD     VALUE '02'.                      ELXPMCCP
00394 ******************************************************************ELXPMCCP
00395 *          MCN-P PARTICIPATION PROVIDER OPTION                    ELXPMCCP
00396 ******************************************************************ELXPMCCP
00397  01  WS-MCNP-PART-IND             PIC X(02).                      ELXPMCCP
00398      88  WS-MCNP-YES             VALUE '01' '02' '03'.            ELXPMCCP
00399      88  WS-MCNP-NO              VALUE '00'.                      ELXPMCCP
00400                                                                   ELXPMCCP
00401 * DETERMINE IF PROVIDER IS IN OR OUT OF NETWORK.                  ELXPMCCP
00402                                                                   ELXPMCCP
00403      88  WS-MCNP-STD             VALUE '01' '03'.                 ELXPMCCP
00404      88  WS-MCNP-CALL            VALUE '02'.                      ELXPMCCP
00405 /                                                                 ELXPMCCP
00406  LINKAGE SECTION.                                                 ELXPMCCP
00407  01  DFHCOMMAREA.                                                 ELXPMCCP
00408      COPY ELSCOMMC.                                               ELXPMCCP
00409 /                                                                 ELXPMCCP
00410      COPY ELSCIA2C.                                               ELXPMCCP
00411 /                                                                 ELXPMCCP
00412      COPY ELSKEYSC.                                               ELXPMCCP
00413 /                                                                 ELXPMCCP
00414      COPY ELSPMCID.                                               ELXPMCCP
00415 /                                                                 ELXPMCCP
00416      COPY ELSIOPMC.                                               ELXPMCCP
00417 /                                                                 ELXPMCCP
00418  01  GROUP-SPECIFIC-RECORD.                                       ELXPMCCP
00419      COPY GCGROUPC.                                               ELXPMCCP
00420 /                                                                 ELXPMCCP
00421  01  GCCP-TBLR-LIST.                                              ELXPMCCP
00422      COPY GCTGCCPC.                                               ELXPMCCP
00423 /                                                                 ELXPMCCP
00424  01  GPPO-TBLR-LIST.                                              ELXPMCCP
00425      COPY GCTGPPOC.                                               ELXPMCCP
00426 /                                                                 ELXPMCCP
00427  01  GRPO-TBLR-LIST.                                              ELXPMCCP
00428      COPY GCTGRPOC.                                               ELXPMCCP
00429 /                                                                 ELXPMCCP
00430 * CPO TABULAR LIST                                                ELXPMCCP
00431  01  GCPO-TBLR-LIST.                                              ELXPMCCP
00432      COPY GCTGCPOC.                                               ELXPMCCP
00433 * CBL TABULAR LIST   RGO, 4/96.                                   ELXPMCCP
00434  01  GCBL-TBLR-LIST.                                              ELXPMCCP
00435      COPY GCTGCBLC.                                               ELXPMCCP
00436 /                                                                 ELXPMCCP
00437  01  GBAE-TBLR-LIST.                                              ELXPMCCP
00438      COPY GCTGBAEC.                                               ELXPMCCP
00439 /                                                                 ELXPMCCP
00440  01  PMCI-COMM-AREA.                                              ELXPMCCP
00441      COPY PMCCOMM.                                                ELXPMCCP
00442 /                                                                 ELXPMCCP
00443      EJECT                                                        ELXPMCCP
00444  PROCEDURE DIVISION.                                              ELXPMCCP
00445 ************************************************************      ELXPMCCP
00446 *                                                          *      ELXPMCCP
00447 *                    PROCEDURE DIVISION                    *      ELXPMCCP
00448 *                                                          *      ELXPMCCP
00449 ************************************************************      ELXPMCCP
00450                                                                   ELXPMCCP
00451                                                                   ELXPMCCP
00452 ************************************************************      ELXPMCCP
00453 *                                                          *      ELXPMCCP
00454 *            COST CONTAINMENT PROGRAMS                     *      ELXPMCCP
00455 *    FOR NOTICE OF ADMISSION/ELIGIBILITY SUMMARY           *      ELXPMCCP
00456 ************************************************************      ELXPMCCP
00457                                                                   ELXPMCCP
00458  0000-CCP-MAINLINE.                                               ELXPMCCP
00459      SET SW-NO-TRMNL-ERR TO TRUE.                                 ELXPMCCP
00460      IF ECA-CIA-PTR = NULL                                        ELXPMCCP
00461         CONTINUE                                                  ELXPMCCP
00462      ELSE                                                         ELXPMCCP
00463         PERFORM 0115-ESTABLISH-ADDRESSIBILITY                     ELXPMCCP
00464         IF SW-NO-TRMNL-ERR                                        ELXPMCCP
00465            PERFORM 0500-TAG-COST-CONTAINMENT-PROG                 ELXPMCCP
00466         ELSE                                                      ELXPMCCP
00467            CONTINUE                                               ELXPMCCP
00468         END-IF                                                    ELXPMCCP
00469      END-IF.                                                      ELXPMCCP
00470      GOBACK.                                                      ELXPMCCP
00471                                                                   ELXPMCCP
00472  0115-ESTABLISH-ADDRESSIBILITY.                                   ELXPMCCP
00473      CALL 'ELUINISM' USING DFHCOMMAREA                            ELXPMCCP
00474                     ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.     ELXPMCCP
00475      SET CIA-PMCCOMM-DDN TO TRUE.                                 ELXPMCCP
00476      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCCP
00477                           ADDRESS OF PMCI-COMM-AREA.              ELXPMCCP
00478      IF CIA-RC-PTR-NULL                                           ELXPMCCP
00479         SET SW-TRMNL-ERR TO TRUE                                  ELXPMCCP
00480         MOVE +1005 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCCP
00481         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCCP
00482      ELSE                                                         ELXPMCCP
00483         SET PMCI-BC-SUCCESSFUL                                    ELXPMCCP
00484             PMCI-BC-NO-ERROR                                      ELXPMCCP
00485             SW-NO-TRMNL-ERR TO TRUE.                              ELXPMCCP
00486      IF SW-NO-TRMNL-ERR                                           ELXPMCCP
00487         SET CIA-ELSGRPSP-DDN TO TRUE                              ELXPMCCP
00488         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCCP
00489                              ADDRESS OF GROUP-SPECIFIC-RECORD     ELXPMCCP
00490         IF CIA-RC-OK                                              ELXPMCCP
00491            PERFORM 0116-ADDRESS-REMAINING-AREAS                   ELXPMCCP
00492         ELSE                                                      ELXPMCCP
00493            SET SW-TRMNL-ERR TO TRUE                               ELXPMCCP
00494            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCCP
00495            MOVE +1001 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCCP
00496         END-IF                                                    ELXPMCCP
00497      END-IF.                                                      ELXPMCCP
00498                                                                   ELXPMCCP
00499  0116-ADDRESS-REMAINING-AREAS.                                    ELXPMCCP
00500      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELXPMCCP
00501      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCCP
00502                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELXPMCCP
00503      IF CIA-RC-PTR-NULL                                           ELXPMCCP
00504         SET CIA-STG-GETMAIN TO TRUE                               ELXPMCCP
00505         CALL 'ELUSTGMG' USING DFHEIBLK                            ELXPMCCP
00506                               DFHCOMMAREA                         ELXPMCCP
00507         SET CIA-ELSKEYS-DDN TO TRUE                               ELXPMCCP
00508         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCCP
00509                      ADDRESS OF KWA-FILE-KEY-WORK-AREA            ELXPMCCP
00510         IF CIA-RC-PTR-NULL                                        ELXPMCCP
00511            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCCP
00512            MOVE +1003 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCCP
00513            SET SW-TRMNL-ERR TO TRUE                               ELXPMCCP
00514         ELSE                                                      ELXPMCCP
00515            SET SW-NO-TRMNL-ERR TO TRUE                            ELXPMCCP
00516         END-IF                                                    ELXPMCCP
00517      END-IF.                                                      ELXPMCCP
00518      IF SW-NO-TRMNL-ERR                                           ELXPMCCP
00519         SET CIA-ELSPMCID-DDN TO TRUE                              ELXPMCCP
00520         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCCP
00521                         ADDRESS OF NAES-INTERMEDIATE-DATA         ELXPMCCP
00522         IF CIA-RC-PTR-NULL                                        ELXPMCCP
00523            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCCP
00524            MOVE +1004 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCCP
00525            SET SW-TRMNL-ERR TO TRUE                               ELXPMCCP
00526         END-IF                                                    ELXPMCCP
00527      END-IF.                                                      ELXPMCCP
00528      IF SW-NO-TRMNL-ERR                                           ELXPMCCP
00529         SET CIA-GCTABULR-DDN TO TRUE                              ELXPMCCP
00530         PERFORM 9100-CALL-STRG-MNGR                               ELXPMCCP
00531         SET CIA-GCTABULR-DDN TO TRUE                              ELXPMCCP
00532         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCCP
00533                     ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS        ELXPMCCP
00534         IF CIA-RC-PTR-NULL                                        ELXPMCCP
00535            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCCP
00536            MOVE +1011 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCCP
00537            SET SW-TRMNL-ERR TO TRUE                               ELXPMCCP
00538         END-IF                                                    ELXPMCCP
00539      END-IF.                                                      ELXPMCCP
00540                                                                   ELXPMCCP
00541 ************************************************************      ELXPMCCP
00542 *0500-TAG-COST-CONTAINMENT-PROG                            *      ELXPMCCP
00543 *   THE PROCESSING FOR ALL FIELDS/PROGRAMS EXCEPT PPO      *      ELXPMCCP
00544 *   FOLLOWS THE SAME LOGIC.                                *      ELXPMCCP
00545 *   NOTE: IN PARAGRAPHS 1000 THRU 6700, THERE IS SOME      *      ELXPMCCP
00546 *         REDUNDANT CODE UNDER THE EVALUATE'S              *      ELXPMCCP
00547 *         (WHEN 'NO' AND WHEN 'CALL'). THIS WAS DONE       *      ELXPMCCP
00548 *         FOR EASE IN UNDERSTANDING AND MAINTANCE.         *      ELXPMCCP
00549 *                                                          *      ELXPMCCP
00550 * ADD CODE FOR CPO. 3/95. RGO                              *      ELXPMCCP
00551 ************************************************************      ELXPMCCP
00552  0500-TAG-COST-CONTAINMENT-PROG.                                  ELXPMCCP
00553      PERFORM 0600-GET-GCCP-RECORD.                                ELXPMCCP
00554      PERFORM 1000-TAG-ADDN-TRANS-CVG.                             ELXPMCCP
00555      PERFORM 2000-TAG-HOSPICE-PROGRAM.                            ELXPMCCP
00556      PERFORM 3000-TAG-MASOP-PROGRAM.                              ELXPMCCP
00557      PERFORM 3500-TAG-MEDNEC-PROGRAM.                             ELXPMCCP
00558      PERFORM 4000-TAG-MOPS-PROGRAM.                               ELXPMCCP
00559      PERFORM 5000-TAG-MSA-PROGRAM.                                ELXPMCCP
00560      SET PMCI-PRG-NO TO TRUE.                                     ELXPMCCP
00561      SET PMCI-PRV-NONE TO TRUE.                                   ELXPMCCP
00562      IF GCG-CBL-PARTICIPATION-IND NOT EQUAL '00'                  ELXPMCCP
00563         PERFORM 6900-TAG-CBL-PROGRAM                              ELXPMCCP
00564      ELSE                                                         ELXPMCCP
00565      IF GCG-CPO-PARTICIPATION-IND EQUAL '01'                      ELXPMCCP
00566         PERFORM 6500-TAG-CPO-PROGRAM                              ELXPMCCP
00567      ELSE                                                         ELXPMCCP
00568      IF GCG-RPO-INDICATOR NOT EQUAL ZERO                          ELXPMCCP
00569         PERFORM 6400-TAG-RPO-PROGRAM                              ELXPMCCP
00570      ELSE                                                         ELXPMCCP
00571      IF GCG-PARTICIPAT-PROV-OPTION NOT EQUAL ZERO                 ELXPMCCP
00572         PERFORM 6000-TAG-PPO-PROGRAM                              ELXPMCCP
00573      ELSE                                                         ELXPMCCP
00574      IF GCG-NEW-POS-IND NOT EQUAL ZERO                            ELXPMCCP
00575         PERFORM 6700-TAG-MCNP-PROGRAM.                            ELXPMCCP
00576      IF GCG-BAE-INDICATOR NOT EQUAL ZERO                          ELXPMCCP
00577                 AND PMCI-PROFESSIONAL                             ELXPMCCP
00578         PERFORM 8200-TAG-BAE-PROGRAM.                             ELXPMCCP
00579      IF PMCI-BC-SUCCESSFUL                                        ELXPMCCP
00580         PERFORM 7000-TAG-PAR-PROGRAM                              ELXPMCCP
00581         PERFORM 8000-TAG-WEEKEND-ADMISSION.                       ELXPMCCP
00582                                                                   ELXPMCCP
00583 ************************************************************      ELXPMCCP
00584  0600-GET-GCCP-RECORD.                                            ELXPMCCP
00585      MOVE SPACES TO KWA-PROVISION-ID.                             ELXPMCCP
00586      SET GCG-INDEX TO 1.                                          ELXPMCCP
00587      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELXPMCCP
00588          AT END                                                   ELXPMCCP
00589             MOVE ZEROES TO KWA-PROVISION-SLOT-NO                  ELXPMCCP
00590             WHEN GCG-TAB-ID (GCG-INDEX) = WS-GCCP                 ELXPMCCP
00591                 MOVE GCG-TAB-ID (GCG-INDEX) TO                    ELXPMCCP
00592          KWA-PROVISION-ID                                         ELXPMCCP
00593                 MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                  ELXPMCCP
00594                     TO KWA-PROVISION-SLOT-NO                      ELXPMCCP
00595          END-SEARCH.                                              ELXPMCCP
00596      IF KWA-PROVISION-SLOT-NO EQUAL ZEROES                        ELXPMCCP
00597         SET WS-GCCP-TAB-NOT-FND TO TRUE                           ELXPMCCP
00598         CONTINUE                                                  ELXPMCCP
00599      ELSE                                                         ELXPMCCP
00600          SET WS-GCCP-TAB-FOUND TO TRUE                            ELXPMCCP
00601          PERFORM 0610-READ-GCCP-RECORD.                           ELXPMCCP
00602                                                                   ELXPMCCP
00603 ************************************************************      ELXPMCCP
00604 *0610-READ-GCCP-PROGRAM                                    *      ELXPMCCP
00605 ************************************************************      ELXPMCCP
00606  0610-READ-GCCP-RECORD.                                           ELXPMCCP
00607      SET CIA-GCTABULR-DDN TO TRUE.                                ELXPMCCP
00608      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCCP
00609                            ADDRESS OF                             ELXPMCCP
00610          IOP-INPUT-OUTPUT-PARAMETERS.                             ELXPMCCP
00611      SET IOP-STG-MODE-MOVE TO TRUE.                               ELXPMCCP
00612      SET IOP-RD              TO TRUE.                             ELXPMCCP
00613      SET IOP-FCQ-NONE        TO TRUE.                             ELXPMCCP
00614      SET IOP-KVQ-EQ          TO TRUE.                             ELXPMCCP
00615      MOVE SPACES TO IOP-AIX-DDNAME.                               ELXPMCCP
00616      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELXPMCCP
00617      PERFORM 9000-CALL-I-O-PGM.                                   ELXPMCCP
00618                                                                   ELXPMCCP
00619 ************************************************************      ELXPMCCP
00620 *1000-TAG-ADDN-TRANS-CVG.                                  *      ELXPMCCP
00621 *   NOTE:THE ATCP FIELD/PROGRAM IS ALSO KNOWN AS HOTS.     *      ELXPMCCP
00622 *                                                          *      ELXPMCCP
00623 ************************************************************      ELXPMCCP
00624  1000-TAG-ADDN-TRANS-CVG.                                         ELXPMCCP
00625      MOVE GCG-ADDL-TRNSPLNT-COVRG-IND TO WS-ATCP-PART-IND.        ELXPMCCP
00626                                                                   ELXPMCCP
00627      IF PMCI-INSTITUTIONAL THEN                                   ELXPMCCP
00628           EVALUATE TRUE                                           ELXPMCCP
00629              WHEN WS-ATCP-INST-NO                                 ELXPMCCP
00630                 SET HOTS-NO TO TRUE                               ELXPMCCP
00631              WHEN WS-ATCP-INST-BSC                                ELXPMCCP
00632                 SET HOTS-YES TO TRUE                              ELXPMCCP
00633                 SET PMCI-HOTS-BSC TO TRUE                         ELXPMCCP
00634              WHEN WS-ATCP-INST-MM                                 ELXPMCCP
00635                 SET HOTS-YES TO TRUE                              ELXPMCCP
00636                 SET PMCI-HOTS-MM  TO TRUE                         ELXPMCCP
00637              WHEN WS-ATCP-INST-BOTH                               ELXPMCCP
00638                 SET HOTS-YES TO TRUE                              ELXPMCCP
00639                 SET PMCI-HOTS-BSC-MM TO TRUE                      ELXPMCCP
00640              WHEN WS-ATCP-CALL                                    ELXPMCCP
00641                 SET HOTS-CALL TO TRUE                             ELXPMCCP
00642              WHEN OTHER                                           ELXPMCCP
00643                 SET HOTS-CALL TO TRUE                             ELXPMCCP
00644           END-EVALUATE                                            ELXPMCCP
00645      ELSE                                                         ELXPMCCP
00646           EVALUATE TRUE                                           ELXPMCCP
00647              WHEN WS-ATCP-PROF-NO                                 ELXPMCCP
00648                 SET HOTS-NO TO TRUE                               ELXPMCCP
00649              WHEN WS-ATCP-PROF-BSC                                ELXPMCCP
00650                 SET HOTS-YES TO TRUE                              ELXPMCCP
00651                 SET PMCI-HOTS-BSC TO TRUE                         ELXPMCCP
00652              WHEN WS-ATCP-PROF-MM                                 ELXPMCCP
00653                 SET HOTS-YES TO TRUE                              ELXPMCCP
00654                 SET PMCI-HOTS-MM  TO TRUE                         ELXPMCCP
00655              WHEN WS-ATCP-PROF-BOTH                               ELXPMCCP
00656                 SET HOTS-YES TO TRUE                              ELXPMCCP
00657                 SET PMCI-HOTS-BSC-MM TO TRUE                      ELXPMCCP
00658              WHEN WS-ATCP-CALL                                    ELXPMCCP
00659                 SET HOTS-CALL TO TRUE                             ELXPMCCP
00660              WHEN OTHER                                           ELXPMCCP
00661                 SET HOTS-CALL TO TRUE                             ELXPMCCP
00662           END-EVALUATE                                            ELXPMCCP
00663      END-IF.                                                      ELXPMCCP
00664 ************************************************************      ELXPMCCP
00665 *2000-TAG-HOSPICE-PROGRAM.                                 *      ELXPMCCP
00666 ************************************************************      ELXPMCCP
00667  2000-TAG-HOSPICE-PROGRAM.                                        ELXPMCCP
00668      MOVE GCG-HOSPICE-IND TO WS-COMMON-PART-IND.                  ELXPMCCP
00669                                                                   ELXPMCCP
00670      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
00671           EVALUATE TRUE                                           ELXPMCCP
00672              WHEN WS-COMMON-INST-NO                               ELXPMCCP
00673                 SET HOSPICE-NO TO TRUE                            ELXPMCCP
00674              WHEN WS-COMMON-INST-BSC                              ELXPMCCP
00675                 SET HOSPICE-YES TO TRUE                           ELXPMCCP
00676                 SET PMCI-HSPC-BSC TO TRUE                         ELXPMCCP
00677              WHEN WS-COMMON-INST-MM                               ELXPMCCP
00678                 SET HOSPICE-YES TO TRUE                           ELXPMCCP
00679                 SET PMCI-HSPC-MM  TO TRUE                         ELXPMCCP
00680              WHEN WS-COMMON-INST-BOTH                             ELXPMCCP
00681                 SET HOSPICE-YES TO TRUE                           ELXPMCCP
00682                 SET PMCI-HSPC-BSC-MM TO TRUE                      ELXPMCCP
00683              WHEN WS-COMMON-CALL                                  ELXPMCCP
00684                 SET HOSPICE-CALL TO TRUE                          ELXPMCCP
00685              WHEN OTHER                                           ELXPMCCP
00686                 SET HOSPICE-CALL TO TRUE                          ELXPMCCP
00687           END-EVALUATE                                            ELXPMCCP
00688      ELSE                                                         ELXPMCCP
00689           EVALUATE TRUE                                           ELXPMCCP
00690              WHEN WS-COMMON-PROF-NO                               ELXPMCCP
00691                 SET HOSPICE-NO TO TRUE                            ELXPMCCP
00692              WHEN WS-COMMON-PROF-BSC                              ELXPMCCP
00693                 SET HOSPICE-YES TO TRUE                           ELXPMCCP
00694                 SET PMCI-HSPC-BSC TO TRUE                         ELXPMCCP
00695              WHEN WS-COMMON-PROF-MM                               ELXPMCCP
00696                 SET HOSPICE-YES TO TRUE                           ELXPMCCP
00697                 SET PMCI-HSPC-MM  TO TRUE                         ELXPMCCP
00698              WHEN WS-COMMON-PROF-BOTH                             ELXPMCCP
00699                 SET HOSPICE-YES TO TRUE                           ELXPMCCP
00700                 SET PMCI-HSPC-BSC-MM TO TRUE                      ELXPMCCP
00701              WHEN WS-COMMON-CALL                                  ELXPMCCP
00702                 SET HOSPICE-CALL TO TRUE                          ELXPMCCP
00703              WHEN OTHER                                           ELXPMCCP
00704                 SET HOSPICE-CALL TO TRUE                          ELXPMCCP
00705           END-EVALUATE                                            ELXPMCCP
00706      END-IF.                                                      ELXPMCCP
00707                                                                   ELXPMCCP
00708 ************************************************************      ELXPMCCP
00709 *3000-TAG-MASOP-PROGRAM.                                   *      ELXPMCCP
00710 ************************************************************      ELXPMCCP
00711  3000-TAG-MASOP-PROGRAM.                                          ELXPMCCP
00712      MOVE GCG-MAND-ADDL-SURG-OPN-IND TO WS-COMMON-PART-IND.       ELXPMCCP
00713                                                                   ELXPMCCP
00714      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
00715           EVALUATE TRUE                                           ELXPMCCP
00716              WHEN WS-COMMON-INST-NO                               ELXPMCCP
00717                 SET PMCI-MASOP-NO TO TRUE                         ELXPMCCP
00718              WHEN WS-COMMON-INST-BSC                              ELXPMCCP
00719                 SET PMCI-MASOP-YES TO TRUE                        ELXPMCCP
00720                 SET PMCI-MASOP-BSC TO TRUE                        ELXPMCCP
00721              WHEN WS-COMMON-INST-MM                               ELXPMCCP
00722                 SET PMCI-MASOP-YES TO TRUE                        ELXPMCCP
00723                 SET PMCI-MASOP-MM  TO TRUE                        ELXPMCCP
00724              WHEN WS-COMMON-INST-BOTH                             ELXPMCCP
00725                 SET PMCI-MASOP-YES TO TRUE                        ELXPMCCP
00726                 SET PMCI-MASOP-BSC-MM TO TRUE                     ELXPMCCP
00727              WHEN WS-COMMON-CALL                                  ELXPMCCP
00728                 SET PMCI-MASOP-CALL TO TRUE                       ELXPMCCP
00729              WHEN OTHER                                           ELXPMCCP
00730                 SET PMCI-MASOP-CALL TO TRUE                       ELXPMCCP
00731           END-EVALUATE                                            ELXPMCCP
00732      ELSE                                                         ELXPMCCP
00733           EVALUATE TRUE                                           ELXPMCCP
00734              WHEN WS-COMMON-PROF-NO                               ELXPMCCP
00735                 SET PMCI-MASOP-NO TO TRUE                         ELXPMCCP
00736              WHEN WS-COMMON-PROF-BSC                              ELXPMCCP
00737                 SET PMCI-MASOP-YES TO TRUE                        ELXPMCCP
00738                 SET PMCI-MASOP-BSC TO TRUE                        ELXPMCCP
00739              WHEN WS-COMMON-PROF-MM                               ELXPMCCP
00740                 SET PMCI-MASOP-YES TO TRUE                        ELXPMCCP
00741                 SET PMCI-MASOP-MM  TO TRUE                        ELXPMCCP
00742              WHEN WS-COMMON-PROF-BOTH                             ELXPMCCP
00743                 SET PMCI-MASOP-YES TO TRUE                        ELXPMCCP
00744                 SET PMCI-MASOP-BSC-MM TO TRUE                     ELXPMCCP
00745              WHEN WS-COMMON-CALL                                  ELXPMCCP
00746                 SET PMCI-MASOP-CALL TO TRUE                       ELXPMCCP
00747              WHEN OTHER                                           ELXPMCCP
00748                 SET PMCI-MASOP-CALL TO TRUE                       ELXPMCCP
00749           END-EVALUATE                                            ELXPMCCP
00750      END-IF.                                                      ELXPMCCP
00751                                                                   ELXPMCCP
00752                                                                   ELXPMCCP
00753 ************************************************************      ELXPMCCP
00754 *3500-TAG-MEDNEC-PROGRAM.                                  *      ELXPMCCP
00755 *            THERE ARE NO CALL VALUES FOR MEDNEC                  ELXPMCCP
00756 ************************************************************      ELXPMCCP
00757  3500-TAG-MEDNEC-PROGRAM.                                         ELXPMCCP
00758      MOVE GCG-MED-NECESSITY-HCNR-IPS-IN TO WS-MEDNEC-PART-IND.    ELXPMCCP
00759                                                                   ELXPMCCP
00760      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
00761           EVALUATE TRUE                                           ELXPMCCP
00762              WHEN WS-MEDNEC-INST-NO                               ELXPMCCP
00763                 SET PMCI-MED-NEC-NO TO TRUE                       ELXPMCCP
00764              WHEN WS-MEDNEC-INST-BSC                              ELXPMCCP
00765                 SET PMCI-MED-NEC-YES TO TRUE                      ELXPMCCP
00766                 SET PMCI-MEDNC-BSC TO TRUE                        ELXPMCCP
00767              WHEN WS-MEDNEC-INST-MM                               ELXPMCCP
00768                 SET PMCI-MED-NEC-YES TO TRUE                      ELXPMCCP
00769                 SET PMCI-MEDNC-MM TO TRUE                         ELXPMCCP
00770              WHEN WS-MEDNEC-INST-BOTH                             ELXPMCCP
00771                 SET PMCI-MED-NEC-YES TO TRUE                      ELXPMCCP
00772                 SET PMCI-MEDNC-BSC-MM TO TRUE                     ELXPMCCP
00773              WHEN OTHER                                           ELXPMCCP
00774                 SET PMCI-MED-NEC-NO TO TRUE                       ELXPMCCP
00775           END-EVALUATE                                            ELXPMCCP
00776      ELSE                                                         ELXPMCCP
00777           EVALUATE TRUE                                           ELXPMCCP
00778              WHEN WS-MEDNEC-PROF-NO                               ELXPMCCP
00779                 SET PMCI-MED-NEC-NO TO TRUE                       ELXPMCCP
00780              WHEN WS-MEDNEC-PROF-BSC                              ELXPMCCP
00781                 SET PMCI-MED-NEC-YES TO TRUE                      ELXPMCCP
00782                 SET PMCI-MEDNC-BSC TO TRUE                        ELXPMCCP
00783              WHEN WS-MEDNEC-PROF-MM                               ELXPMCCP
00784                 SET PMCI-MED-NEC-YES TO TRUE                      ELXPMCCP
00785                 SET PMCI-MEDNC-MM TO TRUE                         ELXPMCCP
00786              WHEN WS-MEDNEC-PROF-BOTH                             ELXPMCCP
00787                 SET PMCI-MED-NEC-YES TO TRUE                      ELXPMCCP
00788                 SET PMCI-MEDNC-BSC-MM TO TRUE                     ELXPMCCP
00789              WHEN OTHER                                           ELXPMCCP
00790                 SET PMCI-MED-NEC-NO TO TRUE                       ELXPMCCP
00791           END-EVALUATE                                            ELXPMCCP
00792      END-IF.                                                      ELXPMCCP
00793                                                                   ELXPMCCP
00794 ************************************************************      ELXPMCCP
00795 *4000-TAG-MOPS-PROGRAM.                                    *      ELXPMCCP
00796 ************************************************************      ELXPMCCP
00797  4000-TAG-MOPS-PROGRAM.                                           ELXPMCCP
00798      MOVE GCG-MAND-OP-SURG-PROG-IND TO WS-MOPS-PART-IND.          ELXPMCCP
00799      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
00800           EVALUATE TRUE                                           ELXPMCCP
00801              WHEN WS-MOPS-INST-NO                                 ELXPMCCP
00802                 SET PMCI-MOPS-NO TO TRUE                          ELXPMCCP
00803              WHEN WS-MOPS-INST-BSC                                ELXPMCCP
00804                 SET PMCI-MOPS-YES TO TRUE                         ELXPMCCP
00805                 SET PMCI-MOPS-BSC TO TRUE                         ELXPMCCP
00806              WHEN WS-MOPS-INST-MM                                 ELXPMCCP
00807                 SET PMCI-MOPS-YES TO TRUE                         ELXPMCCP
00808                 SET PMCI-MOPS-MM TO TRUE                          ELXPMCCP
00809              WHEN WS-MOPS-INST-BOTH                               ELXPMCCP
00810                 SET PMCI-MOPS-YES TO TRUE                         ELXPMCCP
00811                 SET PMCI-MOPS-BSC-MM TO TRUE                      ELXPMCCP
00812              WHEN WS-MOPS-CALL                                    ELXPMCCP
00813                 SET PMCI-MOPS-CALL TO TRUE                        ELXPMCCP
00814              WHEN OTHER                                           ELXPMCCP
00815                 SET PMCI-MOPS-CALL TO TRUE                        ELXPMCCP
00816           END-EVALUATE                                            ELXPMCCP
00817      ELSE                                                         ELXPMCCP
00818           EVALUATE TRUE                                           ELXPMCCP
00819              WHEN WS-MOPS-PROF-NO                                 ELXPMCCP
00820                 SET PMCI-MOPS-NO TO TRUE                          ELXPMCCP
00821              WHEN WS-MOPS-PROF-BSC                                ELXPMCCP
00822                 SET PMCI-MOPS-YES TO TRUE                         ELXPMCCP
00823                 SET PMCI-MOPS-BSC TO TRUE                         ELXPMCCP
00824              WHEN WS-MOPS-PROF-MM                                 ELXPMCCP
00825                 SET PMCI-MOPS-YES TO TRUE                         ELXPMCCP
00826                 SET PMCI-MOPS-MM TO TRUE                          ELXPMCCP
00827              WHEN WS-MOPS-PROF-BOTH                               ELXPMCCP
00828                 SET PMCI-MOPS-YES TO TRUE                         ELXPMCCP
00829                 SET PMCI-MOPS-BSC-MM TO TRUE                      ELXPMCCP
00830              WHEN WS-MOPS-CALL                                    ELXPMCCP
00831                 SET PMCI-MOPS-CALL TO TRUE                        ELXPMCCP
00832              WHEN OTHER                                           ELXPMCCP
00833                 SET PMCI-MOPS-CALL TO TRUE                        ELXPMCCP
00834           END-EVALUATE                                            ELXPMCCP
00835      END-IF.                                                      ELXPMCCP
00836                                                                   ELXPMCCP
00837 ************************************************************      ELXPMCCP
00838 *5000-TAG-MSA-PROGRAM.                                     *      ELXPMCCP
00839 ************************************************************      ELXPMCCP
00840  5000-TAG-MSA-PROGRAM.                                            ELXPMCCP
00841      MOVE GCG-MED-SERV-ADV-PROG-IND TO WS-COMMON-PART-IND.        ELXPMCCP
00842      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
00843         EVALUATE TRUE                                             ELXPMCCP
00844            WHEN WS-COMMON-INST-NO                                 ELXPMCCP
00845               SET PMCI-MSA-NO TO TRUE                             ELXPMCCP
00846            WHEN WS-COMMON-INST-BSC                                ELXPMCCP
00847               SET PMCI-MSA-YES TO TRUE                            ELXPMCCP
00848               SET PMCI-MSA-BSC TO TRUE                            ELXPMCCP
00849            WHEN WS-COMMON-INST-MM                                 ELXPMCCP
00850               SET PMCI-MSA-YES TO TRUE                            ELXPMCCP
00851               SET PMCI-MSA-MM  TO TRUE                            ELXPMCCP
00852            WHEN WS-COMMON-INST-BOTH                               ELXPMCCP
00853               SET PMCI-MSA-YES TO TRUE                            ELXPMCCP
00854               SET PMCI-MSA-BSC-MM TO TRUE                         ELXPMCCP
00855            WHEN WS-COMMON-CALL                                    ELXPMCCP
00856               SET PMCI-MSA-CALL TO TRUE                           ELXPMCCP
00857            WHEN OTHER                                             ELXPMCCP
00858               SET PMCI-MSA-CALL TO TRUE                           ELXPMCCP
00859         END-EVALUATE                                              ELXPMCCP
00860      ELSE                                                         ELXPMCCP
00861         EVALUATE TRUE                                             ELXPMCCP
00862            WHEN WS-COMMON-PROF-NO                                 ELXPMCCP
00863               SET PMCI-MSA-NO TO TRUE                             ELXPMCCP
00864            WHEN WS-COMMON-PROF-BSC                                ELXPMCCP
00865               SET PMCI-MSA-YES TO TRUE                            ELXPMCCP
00866               SET PMCI-MSA-BSC TO TRUE                            ELXPMCCP
00867            WHEN WS-COMMON-PROF-MM                                 ELXPMCCP
00868               SET PMCI-MSA-YES TO TRUE                            ELXPMCCP
00869               SET PMCI-MSA-MM  TO TRUE                            ELXPMCCP
00870            WHEN WS-COMMON-PROF-BOTH                               ELXPMCCP
00871               SET PMCI-MSA-YES TO TRUE                            ELXPMCCP
00872               SET PMCI-MSA-BSC-MM TO TRUE                         ELXPMCCP
00873            WHEN WS-COMMON-CALL                                    ELXPMCCP
00874               SET PMCI-MSA-CALL TO TRUE                           ELXPMCCP
00875            WHEN OTHER                                             ELXPMCCP
00876               SET PMCI-MSA-CALL TO TRUE                           ELXPMCCP
00877         END-EVALUATE                                              ELXPMCCP
00878      END-IF.                                                      ELXPMCCP
00879      IF WS-GCCP-TAB-FOUND                                         ELXPMCCP
00880         IF PMCI-MSA-YES                                           ELXPMCCP
00881            PERFORM 5150-OBTAIN-MSA-IN-GCCP-REC                    ELXPMCCP
00882            IF WS-GCCP-NOT-FOUND                                   ELXPMCCP
00883               SET PMCI-MSA-NO TO TRUE                             ELXPMCCP
00884            ELSE                                                   ELXPMCCP
00885               PERFORM 5175-DTRMN-MSA-APPLS                        ELXPMCCP
00886         ELSE                                                      ELXPMCCP
00887            SET PMCI-MSA-BSC TO TRUE.                              ELXPMCCP
00888                                                                   ELXPMCCP
00889 ************************************************************      ELXPMCCP
00890 *5150-OBTAIN-MSA-IN-GCCP-REC.                              *      ELXPMCCP
00891 ************************************************************      ELXPMCCP
00892  5150-OBTAIN-MSA-IN-GCCP-REC.                                     ELXPMCCP
00893      SET GSS-INDEX TO GSS-ENTRY-COUNT.                            ELXPMCCP
00894      SET WS-GCCP-MAX-IDX TO GSS-INDEX.                            ELXPMCCP
00895      SET WS-GCCP-NOT-FOUND TO TRUE.                               ELXPMCCP
00896      PERFORM WITH TEST BEFORE                                     ELXPMCCP
00897          VARYING GSS-INDEX FROM 1 BY 1                            ELXPMCCP
00898           UNTIL WS-GCCP-FOUND OR                                  ELXPMCCP
00899            GSS-INDEX >  WS-GCCP-MAX-IDX                           ELXPMCCP
00900         IF GSS-MS-PROG-CODE-CHR (GSS-INDEX)                       ELXPMCCP
00901            SET WS-GCCP-FOUND TO TRUE                              ELXPMCCP
00902            SET HOLD-GSS-IDX TO GSS-INDEX                          ELXPMCCP
00903         END-IF                                                    ELXPMCCP
00904      END-PERFORM.                                                 ELXPMCCP
00905 /                                                                 ELXPMCCP
00906 ************************************************************      ELXPMCCP
00907 *5175-DTRMN-MSA-APPLS                                      *      ELXPMCCP
00908 ************************************************************      ELXPMCCP
00909  5175-DTRMN-MSA-APPLS.                                            ELXPMCCP
00910      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
00911         SET GSS-INDEX TO HOLD-GSS-IDX                             ELXPMCCP
00912         MOVE GSS-MS-BC-IND (GSS-INDEX)                            ELXPMCCP
00913                  TO WS-HOLD-BSC-GCCP-IND                          ELXPMCCP
00914         MOVE GSS-MS-MM-IND (GSS-INDEX)                            ELXPMCCP
00915                  TO WS-HOLD-MM-GCCP-IND                           ELXPMCCP
00916         IF PMCI-INPATIENT                                         ELXPMCCP
00917            PERFORM 5180-DTRMN-BC-IP-MSA                           ELXPMCCP
00918         ELSE                                                      ELXPMCCP
00919            PERFORM 5183-DTRMN-BC-OP-MSA                           ELXPMCCP
00920         END-IF                                                    ELXPMCCP
00921      ELSE                                                         ELXPMCCP
00922         IF PMCI-PROFESSIONAL                                      ELXPMCCP
00923            SET GSS-INDEX TO HOLD-GSS-IDX                          ELXPMCCP
00924            MOVE GSS-MS-BS-IND (GSS-INDEX) TO WS-HOLD-BSC-GCCP-IND ELXPMCCP
00925            MOVE GSS-MS-MM-IND (GSS-INDEX) TO WS-HOLD-MM-GCCP-IND  ELXPMCCP
00926            IF PMCI-INPATIENT                                      ELXPMCCP
00927              PERFORM 5185-DTRMN-BS-IP-MSA                         ELXPMCCP
00928            ELSE                                                   ELXPMCCP
00929               PERFORM 5187-DTRMN-BS-OP-MSA                        ELXPMCCP
00930            END-IF                                                 ELXPMCCP
00931         END-IF                                                    ELXPMCCP
00932      END-IF.                                                      ELXPMCCP
00933 /                                                                 ELXPMCCP
00934 ***************************************************************** ELXPMCCP
00935 *5180-DTRMN-BC-IP-OP-MSA                                        * ELXPMCCP
00936 *     SET PMCI-MSA-NO ON ONLY WHEN BOTH INDICATORS ARE ZERO OR  * ELXPMCCP
00937 *     BLANKS. RGO                                                 ELXPMCCP
00938 ***************************************************************** ELXPMCCP
00939  5180-DTRMN-BC-IP-MSA.                                            ELXPMCCP
00940 * CHANGE THE DEFAULT TO CALL - RGO.                               ELXPMCCP
00941      IF WS-BSC-NOT-APPL                                           ELXPMCCP
00942         AND WS-MM-NOT-APPL                                        ELXPMCCP
00943         SET PMCI-MSA-NO TO TRUE                                   ELXPMCCP
00944      ELSE                                                         ELXPMCCP
00945         SET PMCI-MSA-YES TO TRUE                                  ELXPMCCP
00946         IF WS-HOLD-BSC-GCCP-IND NOT EQUAL '00'                    ELXPMCCP
00947            SET PMCI-MSA-BSC TO TRUE                               ELXPMCCP
00948         ELSE SET PMCI-MSA-MM TO TRUE.                             ELXPMCCP
00949                                                                   ELXPMCCP
00950      IF WS-HOLD-BSC-GCCP-IND NOT EQUAL SPACE OR                   ELXPMCCP
00951         WS-HOLD-MM-GCCP-IND NOT EQUAL SPACE                       ELXPMCCP
00952            EVALUATE TRUE                                          ELXPMCCP
00953               WHEN WS-BC-PRCSS-IP-MSA AND                         ELXPMCCP
00954                    WS-MM-PRCSS-IP-MSA                             ELXPMCCP
00955                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
00956                 SET PMCI-MSA-BSC-MM TO TRUE                       ELXPMCCP
00957               WHEN WS-BC-PRCSS-IP-MSA                             ELXPMCCP
00958                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
00959                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
00960               WHEN WS-BC-CALL-IP-MSA                              ELXPMCCP
00961                 SET PMCI-MSA-CALL TO TRUE                         ELXPMCCP
00962                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
00963               WHEN WS-MM-PRCSS-IP-MSA                             ELXPMCCP
00964                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
00965                 SET PMCI-MSA-MM TO TRUE                           ELXPMCCP
00966               WHEN WS-MM-CALL-MSA                                 ELXPMCCP
00967                 SET PMCI-MSA-CALL TO TRUE                         ELXPMCCP
00968            END-EVALUATE                                           ELXPMCCP
00969      END-IF.                                                      ELXPMCCP
00970                                                                   ELXPMCCP
00971 **********************************************************        ELXPMCCP
00972 *5183-DTRMN-BC-OP-MSA                                    *        ELXPMCCP
00973 ************************************************************      ELXPMCCP
00974  5183-DTRMN-BC-OP-MSA.                                            ELXPMCCP
00975 * CHANGE THE DEFAULT TO CALL - RGO.                               ELXPMCCP
00976      IF WS-BSC-NOT-APPL                                           ELXPMCCP
00977         AND WS-MM-NOT-APPL                                        ELXPMCCP
00978         SET PMCI-MSA-NO TO TRUE                                   ELXPMCCP
00979      ELSE                                                         ELXPMCCP
00980         SET PMCI-MSA-YES TO TRUE                                  ELXPMCCP
00981         IF WS-HOLD-BSC-GCCP-IND NOT EQUAL '00'                    ELXPMCCP
00982            SET PMCI-MSA-BSC TO TRUE                               ELXPMCCP
00983         ELSE SET PMCI-MSA-MM TO TRUE.                             ELXPMCCP
00984      IF WS-HOLD-BSC-GCCP-IND NOT EQUAL SPACE OR                   ELXPMCCP
00985         WS-HOLD-MM-GCCP-IND NOT EQUAL SPACE                       ELXPMCCP
00986            EVALUATE TRUE                                          ELXPMCCP
00987               WHEN WS-BC-PRCSS-OP-MSA AND                         ELXPMCCP
00988                    WS-MM-PRCSS-OP-MSA                             ELXPMCCP
00989                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
00990                 SET PMCI-MSA-BSC-MM TO TRUE                       ELXPMCCP
00991               WHEN WS-BC-PRCSS-OP-MSA                             ELXPMCCP
00992                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
00993                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
00994               WHEN WS-BC-CALL-OP-MSA                              ELXPMCCP
00995                 SET PMCI-MSA-CALL TO TRUE                         ELXPMCCP
00996                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
00997               WHEN WS-MM-PRCSS-OP-MSA                             ELXPMCCP
00998                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
00999                 SET PMCI-MSA-MM TO TRUE                           ELXPMCCP
01000               WHEN WS-MM-CALL-MSA                                 ELXPMCCP
01001                 SET PMCI-MSA-CALL TO TRUE                         ELXPMCCP
01002                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
01003            END-EVALUATE                                           ELXPMCCP
01004      END-IF.                                                      ELXPMCCP
01005                                                                   ELXPMCCP
01006 ************************************************************      ELXPMCCP
01007 *5185-DTRMN-BS-IP-MSA                                      *      ELXPMCCP
01008 ************************************************************      ELXPMCCP
01009  5185-DTRMN-BS-IP-MSA.                                            ELXPMCCP
01010 * CHANGE THE DEFAULT TO CALL - RGO.                               ELXPMCCP
01011      IF WS-BSC-NOT-APPL                                           ELXPMCCP
01012         AND WS-MM-NOT-APPL                                        ELXPMCCP
01013         SET PMCI-MSA-NO TO TRUE                                   ELXPMCCP
01014      ELSE                                                         ELXPMCCP
01015         SET PMCI-MSA-YES TO TRUE                                  ELXPMCCP
01016         IF WS-HOLD-BSC-GCCP-IND NOT EQUAL '00'                    ELXPMCCP
01017            SET PMCI-MSA-BSC TO TRUE                               ELXPMCCP
01018         ELSE SET PMCI-MSA-MM TO TRUE.                             ELXPMCCP
01019      IF WS-HOLD-BSC-GCCP-IND NOT EQUAL SPACE OR                   ELXPMCCP
01020         WS-HOLD-MM-GCCP-IND NOT EQUAL SPACE                       ELXPMCCP
01021            EVALUATE TRUE                                          ELXPMCCP
01022               WHEN WS-BS-PRCSS-IP-MSA AND                         ELXPMCCP
01023                    WS-MM-PRCSS-IP-MSA                             ELXPMCCP
01024                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
01025                 SET PMCI-MSA-BSC-MM TO TRUE                       ELXPMCCP
01026               WHEN WS-BS-PRCSS-OP-MSA                             ELXPMCCP
01027                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
01028                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
01029               WHEN WS-BS-CALL-OP-MSA                              ELXPMCCP
01030                 SET PMCI-MSA-CALL TO TRUE                         ELXPMCCP
01031                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
01032               WHEN WS-MM-PRCSS-IP-MSA                             ELXPMCCP
01033                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
01034                 SET PMCI-MSA-MM TO TRUE                           ELXPMCCP
01035               WHEN WS-MM-CALL-MSA                                 ELXPMCCP
01036                 SET PMCI-MSA-CALL TO TRUE                         ELXPMCCP
01037                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
01038            END-EVALUATE                                           ELXPMCCP
01039      END-IF.                                                      ELXPMCCP
01040                                                                   ELXPMCCP
01041 ************************************************************      ELXPMCCP
01042 *5187-DTRMN-BS-OP-MSA                                      *      ELXPMCCP
01043 ************************************************************      ELXPMCCP
01044  5187-DTRMN-BS-OP-MSA.                                            ELXPMCCP
01045 * CHANGE THE DEFAULT TO CALL - RGO.                               ELXPMCCP
01046      IF WS-BSC-NOT-APPL                                           ELXPMCCP
01047         AND WS-MM-NOT-APPL                                        ELXPMCCP
01048         SET PMCI-MSA-NO TO TRUE                                   ELXPMCCP
01049      ELSE                                                         ELXPMCCP
01050         SET PMCI-MSA-YES TO TRUE                                  ELXPMCCP
01051         IF WS-HOLD-BSC-GCCP-IND NOT EQUAL '00'                    ELXPMCCP
01052            SET PMCI-MSA-BSC TO TRUE                               ELXPMCCP
01053         ELSE SET PMCI-MSA-MM TO TRUE.                             ELXPMCCP
01054      IF WS-HOLD-BSC-GCCP-IND NOT EQUAL SPACE OR                   ELXPMCCP
01055         WS-HOLD-MM-GCCP-IND NOT EQUAL SPACE                       ELXPMCCP
01056            EVALUATE TRUE                                          ELXPMCCP
01057               WHEN WS-BS-PRCSS-OP-MSA AND                         ELXPMCCP
01058                    WS-MM-PRCSS-OP-MSA                             ELXPMCCP
01059                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
01060                 SET PMCI-MSA-BSC-MM TO TRUE                       ELXPMCCP
01061               WHEN WS-BS-PRCSS-OP-MSA                             ELXPMCCP
01062                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
01063                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
01064               WHEN WS-BS-CALL-OP-MSA                              ELXPMCCP
01065                 SET PMCI-MSA-CALL TO TRUE                         ELXPMCCP
01066                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
01067               WHEN WS-MM-PRCSS-OP-MSA                             ELXPMCCP
01068                 SET PMCI-MSA-YES TO TRUE                          ELXPMCCP
01069                 SET PMCI-MSA-MM TO TRUE                           ELXPMCCP
01070               WHEN WS-MM-CALL-MSA                                 ELXPMCCP
01071                 SET PMCI-MSA-CALL TO TRUE                         ELXPMCCP
01072                 SET PMCI-MSA-BSC TO TRUE                          ELXPMCCP
01073            END-EVALUATE                                           ELXPMCCP
01074      END-IF.                                                      ELXPMCCP
01075                                                                   ELXPMCCP
01076 ************************************************************      ELXPMCCP
01077 *6000-TAG-PPO-PROGRAM.                                     *      ELXPMCCP
01078 ************************************************************      ELXPMCCP
01079  6000-TAG-PPO-PROGRAM.                                            ELXPMCCP
01080      MOVE GCG-PARTICIPAT-PROV-OPTION TO WS-PPO-PART-IND           ELXPMCCP
01081                                         PMCI-GS-CCP-IND.          ELXPMCCP
01082 *ADDED FOR BLUE STORM                                             ELXPMCCP
01083 *                                                                 ELXPMCCP
01084      IF PMCI-OPERATOR-CHOOSE                                      ELXPMCCP
01085         MOVE '04' TO WS-PPO-PART-IND                              ELXPMCCP
01086 *       SET PMCI-BLUE-STORM-CALL TO TRUE                          ELXPMCCP
01087      END-IF.                                                      ELXPMCCP
01088 *END OF ADDED FOR BLUE STORM                                      ELXPMCCP
01089 *                                                                 ELXPMCCP
01090      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
01091           EVALUATE TRUE                                           ELXPMCCP
01092              WHEN WS-PPO-INST-NO                                  ELXPMCCP
01093                 SET PMCI-PRG-NO TO TRUE                           ELXPMCCP
01094              WHEN WS-PPO-INST-BSC                                 ELXPMCCP
01095                 SET PMCI-PRG-PPO-APPLIES TO TRUE                  ELXPMCCP
01096                 SET PMCI-PRG-BSC TO TRUE                          ELXPMCCP
01097              WHEN WS-PPO-INST-MM                                  ELXPMCCP
01098                 SET PMCI-PRG-PPO-APPLIES TO TRUE                  ELXPMCCP
01099                 SET PMCI-PRG-MM  TO TRUE                          ELXPMCCP
01100              WHEN WS-PPO-INST-BOTH                                ELXPMCCP
01101                 SET PMCI-PRG-PPO-APPLIES TO TRUE                  ELXPMCCP
01102                 SET PMCI-PRG-BSC-MM TO TRUE                       ELXPMCCP
01103              WHEN OTHER                                           ELXPMCCP
01104                 SET PMCI-PRG-CALL TO TRUE                         ELXPMCCP
01105                 SET PMCI-PRV-CALL TO TRUE                         ELXPMCCP
01106           END-EVALUATE                                            ELXPMCCP
01107      ELSE                                                         ELXPMCCP
01108           EVALUATE TRUE                                           ELXPMCCP
01109              WHEN WS-PPO-PROF-NO                                  ELXPMCCP
01110                 SET PMCI-PRG-NO TO TRUE                           ELXPMCCP
01111                 PERFORM 6110-CHECK-FOR-BLUE-STORM                 ELXPMCCP
01112              WHEN WS-PPO-PROF-BSC                                 ELXPMCCP
01113                 SET PMCI-PRG-PPO-APPLIES TO TRUE                  ELXPMCCP
01114                 SET PMCI-PRG-BSC TO TRUE                          ELXPMCCP
01115              WHEN WS-PPO-PROF-MM                                  ELXPMCCP
01116                 SET PMCI-PRG-PPO-APPLIES TO TRUE                  ELXPMCCP
01117                 SET PMCI-PRG-MM  TO TRUE                          ELXPMCCP
01118              WHEN WS-PPO-PROF-BOTH                                ELXPMCCP
01119                 SET PMCI-PRG-PPO-APPLIES TO TRUE                  ELXPMCCP
01120                 SET PMCI-PRG-BSC-MM TO TRUE                       ELXPMCCP
01121              WHEN OTHER                                           ELXPMCCP
01122                 SET PMCI-PRV-CALL TO TRUE                         ELXPMCCP
01123                 SET PMCI-PRG-CALL TO TRUE                         ELXPMCCP
01124           END-EVALUATE                                            ELXPMCCP
01125      END-IF.                                                      ELXPMCCP
01126      IF PMCI-PRG-PPO-APPLIES                                      ELXPMCCP
01127         IF PMCI-INSTITUTIONAL                                     ELXPMCCP
01128            PERFORM 6100-EVAL-INST-PPO-PROV                        ELXPMCCP
01129         ELSE                                                      ELXPMCCP
01130            PERFORM 6200-EVAL-PROF-PPO-PROV.                       ELXPMCCP
01131                                                                   ELXPMCCP
01132                                                                   ELXPMCCP
01133 ***********************************************************       ELXPMCCP
01134 *THIS CODE WITH LET BLURSTORM INTERFACE KNOW THAT                 ELXPMCCP
01135 *PPO APPLIES INST ONLY, BUT TO USE PPO HEADING IF                 ELXPMCCP
01136 *THIS IS A PROFESSIONAL INQUIRY,                                  ELXPMCCP
01137 ***********************************************************       ELXPMCCP
01138  6110-CHECK-FOR-BLUE-STORM.                                       ELXPMCCP
01139      IF WS-PPO-INST-BSC AND PMCI-BLUE-STORM-CALL                  ELXPMCCP
01140         SET PMCI-NEED-PPO-HEADING TO TRUE                         ELXPMCCP
01141      END-IF.                                                      ELXPMCCP
01142                                                                   ELXPMCCP
01143 ***********************************************************       ELXPMCCP
01144 *  DETERMINE IF THE PROVIDER IS A PPO PROVIDER FOR THE            ELXPMCCP
01145 *  PARTICULAR GROUP SECTION BEING SELECTED.  MOST OF THIS IS      ELXPMCCP
01146 *  SELF EXPLANATORY BUT FOR THE NON-STANDARD PPO PARTICIPATION    ELXPMCCP
01147 *  INDICATORS -- 'PPONESS' WILL BE DETERMINED AS FOLLOWS:         ELXPMCCP
01148 *  WHEN PPO-APPLIES A) IF THE PROVIDER IS NOT ON THE GPPO RECORD  ELXPMCCP
01149 *  THEN THIS PROVIDER IS CONSIDERED PPO B) IF THE PROVIDER IS     ELXPMCCP
01150 *  ON THE GPPO RECORD AS EXCLUDED THEN PROVIDER IS NOT CONSIDERED ELXPMCCP
01151 *  PPO  WHEN PPO-NOT-APPLIES BUT PROVIDER IS ON GPPO RECORD AS    ELXPMCCP
01152 *  INCLUDED THEN PROVIDER IS CONSIDERED A PPO PROVIDERS           ELXPMCCP
01153 *                                                                 ELXPMCCP
01154 *  -ADDED THE 'WHEN WS-PPO-INST-CALL' CONDITION.        RGO 6/1/93ELXPMCCP
01155 *                                                                 ELXPMCCP
01156 ******************************************************************ELXPMCCP
01157                                                                   ELXPMCCP
01158  6100-EVAL-INST-PPO-PROV.                                         ELXPMCCP
01159      SET WS-NO-TAB TO TRUE.                                       ELXPMCCP
01160      EVALUATE TRUE                                                ELXPMCCP
01161         WHEN WS-PPO-INST-STD                                      ELXPMCCP
01162            IF PMCI-PPO-PROVIDER                                   ELXPMCCP
01163               SET PMCI-PRV-PPO-IN TO TRUE                         ELXPMCCP
01164            ELSE                                                   ELXPMCCP
01165               SET PMCI-PRV-PPO-OUT TO TRUE                        ELXPMCCP
01166            END-IF                                                 ELXPMCCP
01167         WHEN WS-PPO-INST-NON-STD                                  ELXPMCCP
01168            PERFORM 6300-DETER-PPO-NON-STD                         ELXPMCCP
01169         WHEN WS-PPO-INST-CALL                                     ELXPMCCP
01170            SET PMCI-PRV-CALL TO TRUE                              ELXPMCCP
01171                                                                   ELXPMCCP
01172         WHEN WS-PPO-AMERITECH                                     ELXPMCCP
01173            PERFORM 6320-DETER-PPO-IBT                             ELXPMCCP
01174         WHEN WS-PPO-ZENITH                                        ELXPMCCP
01175            PERFORM 6330-DETER-PPO-ZEN                             ELXPMCCP
01176         WHEN WS-PPO-INST-SPCL                                     ELXPMCCP
01177            PERFORM 6350-DETER-PPO-SPCL                            ELXPMCCP
01178      END-EVALUATE.                                                ELXPMCCP
01179                                                                   ELXPMCCP
01180                                                                   ELXPMCCP
01181 ******************************************************************ELXPMCCP
01182 * 6200-EVAL-PROF-PPO-PROV.                                        ELXPMCCP
01183 *  -ADDED THE 'WHEN WS-PPO-INST-CALL' CONDITION.        RGO 6/1/93ELXPMCCP
01184 *                                                                 ELXPMCCP
01185 ******************************************************************ELXPMCCP
01186  6200-EVAL-PROF-PPO-PROV.                                         ELXPMCCP
01187      SET WS-NO-TAB TO TRUE.                                       ELXPMCCP
01188      EVALUATE TRUE                                                ELXPMCCP
01189         WHEN WS-PPO-PROF-STD                                      ELXPMCCP
01190            IF PMCI-PPO-PROVIDER                                   ELXPMCCP
01191               SET PMCI-PRV-PPO-IN TO TRUE                         ELXPMCCP
01192            ELSE                                                   ELXPMCCP
01193               SET PMCI-PRV-PPO-OUT TO TRUE                        ELXPMCCP
01194            END-IF                                                 ELXPMCCP
01195         WHEN WS-PPO-PROF-MPP                                      ELXPMCCP
01196            IF PMCI-MPP-PROVIDER                                   ELXPMCCP
01197               SET PMCI-PRV-PPO-IN TO TRUE                         ELXPMCCP
01198            ELSE                                                   ELXPMCCP
01199               SET PMCI-PRV-NONE TO TRUE                           ELXPMCCP
01200            END-IF                                                 ELXPMCCP
01201         WHEN WS-PPO-PROF-NON-STD                                  ELXPMCCP
01202            PERFORM 6300-DETER-PPO-NON-STD                         ELXPMCCP
01203         WHEN WS-PPO-PROF-CALL                                     ELXPMCCP
01204            SET PMCI-PRV-CALL TO TRUE                              ELXPMCCP
01205                                                                   ELXPMCCP
01206         WHEN WS-PPO-AMERITECH                                     ELXPMCCP
01207            PERFORM 6320-DETER-PPO-IBT                             ELXPMCCP
01208         WHEN WS-PPO-ZENITH                                        ELXPMCCP
01209            PERFORM 6330-DETER-PPO-ZEN                             ELXPMCCP
01210         WHEN WS-PPO-PROF-SPCL                                     ELXPMCCP
01211            PERFORM 6350-DETER-PPO-SPCL                            ELXPMCCP
01212      END-EVALUATE.                                                ELXPMCCP
01213  6300-DETER-PPO-NON-STD.                                          ELXPMCCP
01214      MOVE '#GPPO ' TO WS-TABULAR.                                 ELXPMCCP
01215      PERFORM 8500-DETER-TAB-SEARCH.                               ELXPMCCP
01216      EVALUATE TRUE                                                ELXPMCCP
01217       WHEN WS-NO-TAB                                              ELXPMCCP
01218         IF PMCI-PPO-PROVIDER                                      ELXPMCCP
01219            SET PMCI-PRV-PPO-IN TO TRUE                            ELXPMCCP
01220         ELSE                                                      ELXPMCCP
01221            IF PMCI-NON-PPO-PROVIDER                               ELXPMCCP
01222               SET PMCI-PRV-PPO-OUT TO TRUE                        ELXPMCCP
01223            ELSE                                                   ELXPMCCP
01224               SET PMCI-BC-INVALID-DATA TO TRUE                    ELXPMCCP
01225               MOVE +1006 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCCP
01226            END-IF                                                 ELXPMCCP
01227         END-IF                                                    ELXPMCCP
01228      WHEN WS-NO-MATCH-TAB                                         ELXPMCCP
01229         IF GSW-CORP-LIST-OVERIDE-IND = 'I'                        ELXPMCCP
01230            SET PMCI-PRV-NONE TO TRUE                              ELXPMCCP
01231         ELSE                                                      ELXPMCCP
01232            IF PMCI-PPO-PROVIDER                                   ELXPMCCP
01233               SET PMCI-PRV-PPO-IN TO TRUE                         ELXPMCCP
01234            ELSE                                                   ELXPMCCP
01235               IF PMCI-NON-PPO-PROVIDER                            ELXPMCCP
01236                  SET PMCI-PRV-PPO-OUT TO TRUE                     ELXPMCCP
01237               ELSE                                                ELXPMCCP
01238                  SET PMCI-BC-INVALID-DATA TO TRUE                 ELXPMCCP
01239                  MOVE +1006 TO PMCI-BLUE-CHIP-ERROR-CODE          ELXPMCCP
01240               END-IF                                              ELXPMCCP
01241            END-IF                                                 ELXPMCCP
01242         END-IF                                                    ELXPMCCP
01243      WHEN WS-TAB-INCLUDE                                          ELXPMCCP
01244         SET PMCI-PRV-PPO-IN TO TRUE                               ELXPMCCP
01245      WHEN WS-TAB-EXCLUDE                                          ELXPMCCP
01246         SET PMCI-PRV-PPO-OUT TO TRUE                              ELXPMCCP
01247      WHEN OTHER                                                   ELXPMCCP
01248         MOVE +1007 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCCP
01249         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCCP
01250      END-EVALUATE.                                                ELXPMCCP
01251                                                                   ELXPMCCP
01252  6320-DETER-PPO-IBT.                                              ELXPMCCP
01253      MOVE '#GPPO ' TO WS-TABULAR.                                 ELXPMCCP
01254      PERFORM 8500-DETER-TAB-SEARCH.                               ELXPMCCP
01255      EVALUATE TRUE                                                ELXPMCCP
01256         WHEN WS-NO-TAB                                            ELXPMCCP
01257            IF PMCI-IBT-PROVIDER                                   ELXPMCCP
01258               SET PMCI-PRV-PPO-IN TO TRUE                         ELXPMCCP
01259            ELSE                                                   ELXPMCCP
01260               SET PMCI-PRV-PPO-OUT TO TRUE                        ELXPMCCP
01261            END-IF                                                 ELXPMCCP
01262         WHEN WS-NO-MATCH-TAB                                      ELXPMCCP
01263            IF GSW-CORP-LIST-OVERIDE-IND = 'I'                     ELXPMCCP
01264               SET PMCI-PRV-PPO-OUT TO TRUE                        ELXPMCCP
01265            ELSE                                                   ELXPMCCP
01266               IF PMCI-IBT-PROVIDER                                ELXPMCCP
01267                  SET PMCI-PRV-PPO-IN TO TRUE                      ELXPMCCP
01268               ELSE                                                ELXPMCCP
01269                  IF PMCI-NON-IBT-PROVIDER                         ELXPMCCP
01270                     SET PMCI-PRV-PPO-OUT TO TRUE                  ELXPMCCP
01271                  ELSE                                             ELXPMCCP
01272                     MOVE +1006 TO PMCI-BLUE-CHIP-ERROR-CODE       ELXPMCCP
01273                     SET PMCI-BC-INVALID-DATA TO TRUE              ELXPMCCP
01274                  END-IF                                           ELXPMCCP
01275               END-IF                                              ELXPMCCP
01276            END-IF                                                 ELXPMCCP
01277         WHEN WS-TAB-EXCLUDE                                       ELXPMCCP
01278            SET PMCI-PRV-PPO-OUT TO TRUE                           ELXPMCCP
01279         WHEN WS-TAB-INCLUDE                                       ELXPMCCP
01280            SET PMCI-PRV-PPO-IN TO TRUE                            ELXPMCCP
01281         WHEN OTHER                                                ELXPMCCP
01282            MOVE +1007 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCCP
01283            SET PMCI-BC-INVALID-DATA TO TRUE                       ELXPMCCP
01284      END-EVALUATE.                                                ELXPMCCP
01285                                                                   ELXPMCCP
01286  6330-DETER-PPO-ZEN.                                              ELXPMCCP
01287      MOVE '#GPPO ' TO WS-TABULAR.                                 ELXPMCCP
01288      PERFORM 8500-DETER-TAB-SEARCH.                               ELXPMCCP
01289      IF (PMCI-ZEN-PROVIDER OR PMCI-PPO-PROVIDER)                  ELXPMCCP
01290           AND WS-NO-MATCH-TAB                                     ELXPMCCP
01291         SET PMCI-PRV-PPO-IN TO TRUE                               ELXPMCCP
01292      ELSE                                                         ELXPMCCP
01293         IF (PMCI-ZEN-PROVIDER OR PMCI-PPO-PROVIDER)               ELXPMCCP
01294            AND WS-NO-TAB                                          ELXPMCCP
01295            SET PMCI-PRV-PPO-IN TO TRUE                            ELXPMCCP
01296         ELSE                                                      ELXPMCCP
01297            IF (PMCI-ZEN-PROVIDER OR PMCI-PPO-PROVIDER)            ELXPMCCP
01298              AND WS-TAB-EXCLUDE                                   ELXPMCCP
01299               SET PMCI-PRV-PPO-OUT TO TRUE                        ELXPMCCP
01300            ELSE                                                   ELXPMCCP
01301               IF (PMCI-ZEN-PROVIDER OR PMCI-PPO-PROVIDER)         ELXPMCCP
01302                    AND WS-TAB-INCLUDE                             ELXPMCCP
01303                 SET PMCI-PRV-PPO-IN TO TRUE                       ELXPMCCP
01304               ELSE                                                ELXPMCCP
01305                  IF (NOT PMCI-ZEN-PROVIDER                        ELXPMCCP
01306                        OR NOT PMCI-PPO-PROVIDER)                  ELXPMCCP
01307                        AND WS-TAB-INCLUDE                         ELXPMCCP
01308                     SET PMCI-PRV-PPO-IN TO TRUE                   ELXPMCCP
01309                  ELSE                                             ELXPMCCP
01310                     SET PMCI-PRV-PPO-OUT TO TRUE                  ELXPMCCP
01311                  END-IF                                           ELXPMCCP
01312            END-IF                                                 ELXPMCCP
01313         END-IF                                                    ELXPMCCP
01314      END-IF.                                                      ELXPMCCP
01315                                                                   ELXPMCCP
01316                                                                   ELXPMCCP
01317  6350-DETER-PPO-SPCL.                                             ELXPMCCP
01318      MOVE '#GPPO ' TO WS-TABULAR.                                 ELXPMCCP
01319      PERFORM 8500-DETER-TAB-SEARCH.                               ELXPMCCP
01320      IF WS-TAB-INCLUDE                                            ELXPMCCP
01321         SET PMCI-PRV-PPO-IN TO TRUE                               ELXPMCCP
01322      ELSE                                                         ELXPMCCP
01323         IF WS-TAB-EXCLUDE                                         ELXPMCCP
01324            SET PMCI-PRV-PPO-OUT TO TRUE                           ELXPMCCP
01325         ELSE                                                      ELXPMCCP
01326            IF WS-NO-MATCH-TAB                                     ELXPMCCP
01327              SET PMCI-PRV-CALL TO TRUE                            ELXPMCCP
01328            ELSE                                                   ELXPMCCP
01329               MOVE +1007 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCCP
01330               SET PMCI-BC-INVALID-DATA TO TRUE                    ELXPMCCP
01331            END-IF                                                 ELXPMCCP
01332         END-IF                                                    ELXPMCCP
01333      END-IF.                                                      ELXPMCCP
01334 ************************************************************      ELXPMCCP
01335 *6400-TAG-RPO-PROGRAM.                                     *      ELXPMCCP
01336 ************************************************************      ELXPMCCP
01337  6400-TAG-RPO-PROGRAM.                                            ELXPMCCP
01338      MOVE GCG-RPO-INDICATOR TO WS-RPO-PART-IND.                   ELXPMCCP
01339                                                                   ELXPMCCP
01340      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
01341           EVALUATE TRUE                                           ELXPMCCP
01342              WHEN WS-RPO-INST-NO                                  ELXPMCCP
01343                 SET PMCI-PRG-NO TO TRUE                           ELXPMCCP
01344              WHEN WS-RPO-INST-YES                                 ELXPMCCP
01345                 SET PMCI-PRG-RPO-APPLIES TO TRUE                  ELXPMCCP
01346                 PERFORM 6800-DETERMINE-LOB-PRV                    ELXPMCCP
01347                 PERFORM 6450-EVAL-INST-RPO-PROV                   ELXPMCCP
01348              WHEN OTHER                                           ELXPMCCP
01349                 SET PMCI-PRG-CALL TO TRUE                         ELXPMCCP
01350                 SET PMCI-PRV-CALL TO TRUE                         ELXPMCCP
01351           END-EVALUATE                                            ELXPMCCP
01352      ELSE                                                         ELXPMCCP
01353      IF PMCI-PROFESSIONAL                                         ELXPMCCP
01354           CONTINUE.                                               ELXPMCCP
01355 ************************************************************      ELXPMCCP
01356 *6450-EVALUATE INSTITUTIONAL RPO PROVIDER                  *      ELXPMCCP
01357 ************************************************************      ELXPMCCP
01358  6450-EVAL-INST-RPO-PROV.                                         ELXPMCCP
01359      SET WS-NO-TAB TO TRUE.                                       ELXPMCCP
01360      EVALUATE TRUE                                                ELXPMCCP
01361         WHEN WS-RPO-INST-STD                                      ELXPMCCP
01362            IF PMCI-RPO-PROVIDER                                   ELXPMCCP
01363               SET PMCI-PRV-RPO-IN TO TRUE                         ELXPMCCP
01364            ELSE                                                   ELXPMCCP
01365               SET PMCI-PRV-RPO-OUT TO TRUE                        ELXPMCCP
01366            END-IF                                                 ELXPMCCP
01367         WHEN WS-RPO-INST-NON-STD                                  ELXPMCCP
01368            PERFORM 6460-DETER-RPO-NON-STD                         ELXPMCCP
01369      END-EVALUATE.                                                ELXPMCCP
01370      IF PMCI-PRV-RPO-OUT AND                                      ELXPMCCP
01371               WS-RPO-INST-NON-STD                                 ELXPMCCP
01372         PERFORM 6470-TEST-RPO-IN-PPO.                             ELXPMCCP
01373                                                                   ELXPMCCP
01374 ************************************************************      ELXPMCCP
01375 *6460-EVALUATE NON STANDARD RPO                            *      ELXPMCCP
01376 ************************************************************      ELXPMCCP
01377  6460-DETER-RPO-NON-STD.                                          ELXPMCCP
01378      MOVE '#GRPO ' TO WS-TABULAR.                                 ELXPMCCP
01379      PERFORM 8500-DETER-TAB-SEARCH.                               ELXPMCCP
01380      EVALUATE TRUE                                                ELXPMCCP
01381       WHEN WS-NO-TAB                                              ELXPMCCP
01382         IF PMCI-RPO-PROVIDER                                      ELXPMCCP
01383            SET PMCI-PRV-RPO-IN TO TRUE                            ELXPMCCP
01384         ELSE                                                      ELXPMCCP
01385            IF PMCI-NON-RPO-PROVIDER                               ELXPMCCP
01386               SET PMCI-PRV-RPO-OUT TO TRUE                        ELXPMCCP
01387            ELSE                                                   ELXPMCCP
01388               SET PMCI-BC-INVALID-DATA TO TRUE                    ELXPMCCP
01389               MOVE +1012 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCCP
01390            END-IF                                                 ELXPMCCP
01391         END-IF                                                    ELXPMCCP
01392      WHEN WS-NO-MATCH-TAB                                         ELXPMCCP
01393         IF GS7-CORP-LIST-OVERIDE-IND = 'I'                        ELXPMCCP
01394            SET PMCI-PRV-RPO-OUT TO TRUE                           ELXPMCCP
01395         ELSE                                                      ELXPMCCP
01396            IF PMCI-RPO-PROVIDER                                   ELXPMCCP
01397               SET PMCI-PRV-RPO-IN TO TRUE                         ELXPMCCP
01398            ELSE                                                   ELXPMCCP
01399               IF PMCI-NON-RPO-PROVIDER                            ELXPMCCP
01400                  SET PMCI-PRV-RPO-OUT TO TRUE                     ELXPMCCP
01401               ELSE                                                ELXPMCCP
01402                  SET PMCI-BC-INVALID-DATA TO TRUE                 ELXPMCCP
01403                  MOVE +1012 TO PMCI-BLUE-CHIP-ERROR-CODE          ELXPMCCP
01404               END-IF                                              ELXPMCCP
01405            END-IF                                                 ELXPMCCP
01406         END-IF                                                    ELXPMCCP
01407      WHEN WS-TAB-INCLUDE                                          ELXPMCCP
01408         SET PMCI-PRV-RPO-IN TO TRUE                               ELXPMCCP
01409      WHEN WS-TAB-EXCLUDE                                          ELXPMCCP
01410         SET PMCI-PRV-RPO-OUT TO TRUE                              ELXPMCCP
01411      WHEN OTHER                                                   ELXPMCCP
01412         MOVE +1013 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCCP
01413         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCCP
01414      END-EVALUATE.                                                ELXPMCCP
01415                                                                   ELXPMCCP
01416 ************************************************************      ELXPMCCP
01417 *6470-TEST-RPO-IN-PPO.                                      *     ELXPMCCP
01418 ************************************************************      ELXPMCCP
01419                                                                   ELXPMCCP
01420  6470-TEST-RPO-IN-PPO.                                            ELXPMCCP
01421                                                                   ELXPMCCP
01422      IF WS-GCCP-TAB-FOUND                                         ELXPMCCP
01423         PERFORM 6475-OBTAIN-RPO-IN-GCCP-REC                       ELXPMCCP
01424         IF WS-GCCP-FOUND                                          ELXPMCCP
01425            IF GSS-RP-BC-PAYMENT-LEVEL-IND (GSS-INDEX) = '01'      ELXPMCCP
01426               PERFORM 6480-TEST-PPO-FOR-RPO.                      ELXPMCCP
01427                                                                   ELXPMCCP
01428 ************************************************************      ELXPMCCP
01429 *6475-OBTAIN-RPO-IN-GCCP-REC.                              *      ELXPMCCP
01430 ************************************************************      ELXPMCCP
01431                                                                   ELXPMCCP
01432  6475-OBTAIN-RPO-IN-GCCP-REC.                                     ELXPMCCP
01433                                                                   ELXPMCCP
01434      SET GSS-INDEX TO GSS-ENTRY-COUNT.                            ELXPMCCP
01435      SET WS-GCCP-MAX-IDX TO GSS-INDEX.                            ELXPMCCP
01436      SET WS-GCCP-NOT-FOUND TO TRUE.                               ELXPMCCP
01437      PERFORM WITH TEST BEFORE                                     ELXPMCCP
01438          VARYING GSS-INDEX FROM 1 BY 1                            ELXPMCCP
01439           UNTIL WS-GCCP-FOUND OR                                  ELXPMCCP
01440            GSS-INDEX >  WS-GCCP-MAX-IDX                           ELXPMCCP
01441         IF GSS-RP-PROG-CODE-CHR (GSS-INDEX)                       ELXPMCCP
01442            SET WS-GCCP-FOUND TO TRUE                              ELXPMCCP
01443            SET HOLD-GSS-IDX TO GSS-INDEX                          ELXPMCCP
01444         END-IF                                                    ELXPMCCP
01445      END-PERFORM.                                                 ELXPMCCP
01446 ************************************************************      ELXPMCCP
01447 *6480-TEST-PPO-FOR-RPO                                            ELXPMCCP
01448 ************************************************************      ELXPMCCP
01449                                                                   ELXPMCCP
01450  6480-TEST-PPO-FOR-RPO.                                           ELXPMCCP
01451      MOVE GCG-PARTICIPAT-PROV-OPTION TO WS-PPO-PART-IND.          ELXPMCCP
01452      SET WS-NO-TAB TO TRUE.                                       ELXPMCCP
01453      EVALUATE TRUE                                                ELXPMCCP
01454         WHEN WS-PPO-INST-STD                                      ELXPMCCP
01455            IF PMCI-RPO-PROVIDER                                   ELXPMCCP
01456               SET PMCI-PRV-RPO-IN-PPO TO TRUE                     ELXPMCCP
01457            END-IF                                                 ELXPMCCP
01458         WHEN WS-PPO-INST-NON-STD                                  ELXPMCCP
01459            PERFORM 6490-DETER-PPO-NON-STD                         ELXPMCCP
01460         WHEN WS-PPO-INST-CALL                                     ELXPMCCP
01461            SET PMCI-PRV-CALL TO TRUE                              ELXPMCCP
01462      END-EVALUATE.                                                ELXPMCCP
01463                                                                   ELXPMCCP
01464 ************************************************************      ELXPMCCP
01465 *6490-DETER-PPO-NON-STD.                                   *      ELXPMCCP
01466 ************************************************************      ELXPMCCP
01467                                                                   ELXPMCCP
01468  6490-DETER-PPO-NON-STD.                                          ELXPMCCP
01469      MOVE '#GPPO ' TO WS-TABULAR.                                 ELXPMCCP
01470      PERFORM 8500-DETER-TAB-SEARCH.                               ELXPMCCP
01471      EVALUATE TRUE                                                ELXPMCCP
01472      WHEN WS-TAB-INCLUDE                                          ELXPMCCP
01473         SET PMCI-PRV-RPO-IN-PPO TO TRUE                           ELXPMCCP
01474      WHEN OTHER                                                   ELXPMCCP
01475         CONTINUE                                                  ELXPMCCP
01476      END-EVALUATE.                                                ELXPMCCP
01477                                                                   ELXPMCCP
01478 ******************************************************************ELXPMCCP
01479 *6500-TAG-CPO-PROGRAM.                                           *ELXPMCCP
01480 *  3 LEVELS OF CPO BENEFITS:                                     *ELXPMCCP
01481 *    1) THE SUBSCRIBER HAS CPO AND WENT TO A CPO PROVIDER.       *ELXPMCCP
01482 *    2) THEY WENT A PPO PROVIDER.                                *ELXPMCCP
01483 *    3) THEY WENT TOTALLY OUT OF NETWORK.                        *ELXPMCCP
01484 ******************************************************************ELXPMCCP
01485  6500-TAG-CPO-PROGRAM.                                            ELXPMCCP
01486                                                                   ELXPMCCP
01487                                                                   ELXPMCCP
01488      IF PMCI-CPO-INDICATOR = '1'                                  ELXPMCCP
01489         SET PMCI-PRG-CPO-APPLIES TO TRUE                          ELXPMCCP
01490         SET PMCI-PRV-CPO-MET TO TRUE                              ELXPMCCP
01491      ELSE                                                         ELXPMCCP
01492         SET PMCI-PRG-PPO-APPLIES TO TRUE                          ELXPMCCP
01493         IF PMCI-PPO-INDICATOR = '1'                               ELXPMCCP
01494               SET PMCI-PRV-CPO-PPO-MET TO TRUE                    ELXPMCCP
01495         ELSE SET PMCI-PRV-CPO-PPO-NOT-MET TO TRUE                 ELXPMCCP
01496         END-IF                                                    ELXPMCCP
01497      END-IF.                                                      ELXPMCCP
01498 ************************************************************      ELXPMCCP
01499 *6700-TAG-MCNP-PROGRAM.                                     *     ELXPMCCP
01500 ************************************************************      ELXPMCCP
01501  6700-TAG-MCNP-PROGRAM.                                           ELXPMCCP
01502      MOVE GCG-NEW-POS-IND TO WS-MCNP-PART-IND.                    ELXPMCCP
01503                                                                   ELXPMCCP
01504           EVALUATE TRUE                                           ELXPMCCP
01505              WHEN WS-MCNP-NO                                      ELXPMCCP
01506                 SET PMCI-PRG-NO TO TRUE                           ELXPMCCP
01507              WHEN WS-MCNP-YES                                     ELXPMCCP
01508                 SET PMCI-PRG-MCNP-APPLIES TO TRUE                 ELXPMCCP
01509                 PERFORM 6800-DETERMINE-LOB-PRV                    ELXPMCCP
01510                 PERFORM 6750-EVAL-MCNP-PROV                       ELXPMCCP
01511              WHEN OTHER                                           ELXPMCCP
01512                 SET PMCI-PRG-CALL TO TRUE                         ELXPMCCP
01513                 SET PMCI-PRV-CALL TO TRUE                         ELXPMCCP
01514           END-EVALUATE.                                           ELXPMCCP
01515 ************************************************************      ELXPMCCP
01516 *6750-EVALUATE MCNP PROVIDER                               *      ELXPMCCP
01517 ************************************************************      ELXPMCCP
01518  6750-EVAL-MCNP-PROV.                                             ELXPMCCP
01519      EVALUATE TRUE                                                ELXPMCCP
01520         WHEN WS-MCNP-STD                                          ELXPMCCP
01521            IF PMCI-REFERRAL-EXISTS                                ELXPMCCP
01522               SET PMCI-PRV-MCNP-REFER TO TRUE                     ELXPMCCP
01523            ELSE                                                   ELXPMCCP
01524               IF PMCI-REFERRAL-NOT-REQUIRED                       ELXPMCCP
01525                  SET PMCI-PRV-MCNP-IN TO TRUE                     ELXPMCCP
01526            ELSE                                                   ELXPMCCP
01527               SET PMCI-PRV-MCNP-OUT TO TRUE                       ELXPMCCP
01528               END-IF                                              ELXPMCCP
01529            END-IF                                                 ELXPMCCP
01530         WHEN WS-MCNP-CALL                                         ELXPMCCP
01531            IF PMCI-REFERRAL-EXISTS                                ELXPMCCP
01532               SET PMCI-PRV-MCNP-REFER TO TRUE                     ELXPMCCP
01533            ELSE                                                   ELXPMCCP
01534               SET PMCI-PRV-CALL TO TRUE                           ELXPMCCP
01535            END-IF.                                                ELXPMCCP
01536 ************************************************************      ELXPMCCP
01537 *6800-DETERMINE LINE OF BUSINESS FOR RPO                   *      ELXPMCCP
01538 ************************************************************      ELXPMCCP
01539  6800-DETERMINE-LOB-PRV.                                          ELXPMCCP
01540      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES AND                  ELXPMCCP
01541               PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES                 ELXPMCCP
01542         SET PMCI-PRG-BSC-MM TO TRUE                               ELXPMCCP
01543      ELSE                                                         ELXPMCCP
01544      IF PMCI-BSC-CNTRCT-GRP = SPACES                              ELXPMCCP
01545         SET PMCI-PRG-MM TO TRUE                                   ELXPMCCP
01546      ELSE                                                         ELXPMCCP
01547         SET PMCI-PRG-BSC TO TRUE.                                 ELXPMCCP
01548 ************************************************************      ELXPMCCP
01549 *6900-TAG-CBL-PROGRAM                           RGO        *      ELXPMCCP
01550 *  1. SET THE APPLIES FIELD TO TRUE.                       *      ELXPMCCP
01551 *  2. IF THE PROVIDER IS CBL (SET IN THE SCREEN PROGRAM)   *      ELXPMCCP
01552 *        SET THE 'IN' INDICATOR, ELSE SET THE 'OUT'.       *      ELXPMCCP
01553 *  3. CHECK TO SEE IF THE GROUP SPECIFIC RECORD HAS A      *      ELXPMCCP
01554 *     GCBL TABULAR, AND IF THE PROVIDER IS ONE IT.         *      ELXPMCCP
01555 *                                                          *      ELXPMCCP
01556 *  4. IF THE PROVIDER IS ON THE TABULAR, THEN              *      ELXPMCCP
01557 *     'IN' VS 'OUT' IS DETERMINED BY THIS.                 *      ELXPMCCP
01558 *                                                          *      ELXPMCCP
01559 ************************************************************      ELXPMCCP
01560  6900-TAG-CBL-PROGRAM.                                            ELXPMCCP
01561      SET PMCI-PRG-CBL-APPLIES TO TRUE                             ELXPMCCP
01562      IF PMCI-CBL-INDICATOR = '1'                                  ELXPMCCP
01563         SET PMCI-PRV-CBL-IN TO TRUE                               ELXPMCCP
01564      ELSE SET PMCI-PRV-CBL-OUT TO TRUE.                           ELXPMCCP
01565                                                                   ELXPMCCP
01566      MOVE '#GCBL ' TO WS-TABULAR.                                 ELXPMCCP
01567      PERFORM 8500-DETER-TAB-SEARCH.                               ELXPMCCP
01568                                                                   ELXPMCCP
01569      IF WS-TAB-SWITCH = 'I'                                       ELXPMCCP
01570         SET PMCI-PRV-CBL-IN TO TRUE.                              ELXPMCCP
01571      IF WS-TAB-SWITCH = 'E'                                       ELXPMCCP
01572         SET PMCI-PRV-CBL-OUT TO TRUE.                             ELXPMCCP
01573 ************************************************************      ELXPMCCP
01574 *7000 TAG PAR PROGRAM                                             ELXPMCCP
01575 ************************************************************      ELXPMCCP
01576  7000-TAG-PAR-PROGRAM.                                            ELXPMCCP
01577      MOVE GCG-PRE-ADM-REVIEW-IND TO WS-PAR-PART-IND.              ELXPMCCP
01578      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
01579           EVALUATE TRUE                                           ELXPMCCP
01580              WHEN WS-PAR-INST-NO                                  ELXPMCCP
01581                 SET PRE-CERT-DOES-NOT-APPLY TO TRUE               ELXPMCCP
01582              WHEN WS-PAR-INST-BSC                                 ELXPMCCP
01583                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01584                 SET PMCI-PAR-BSC TO TRUE                          ELXPMCCP
01585              WHEN WS-PAR-INST-MM                                  ELXPMCCP
01586                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01587                 SET PMCI-PAR-MM TO TRUE                           ELXPMCCP
01588              WHEN WS-PAR-INST-BOTH                                ELXPMCCP
01589                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01590                 SET PMCI-PAR-BSC-MM TO TRUE                       ELXPMCCP
01591              WHEN WS-PAR-CALL                                     ELXPMCCP
01592                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01593              WHEN OTHER                                           ELXPMCCP
01594                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01595           END-EVALUATE                                            ELXPMCCP
01596      ELSE                                                         ELXPMCCP
01597           EVALUATE TRUE                                           ELXPMCCP
01598              WHEN WS-PAR-PROF-NO                                  ELXPMCCP
01599                 SET PRE-CERT-DOES-NOT-APPLY TO TRUE               ELXPMCCP
01600              WHEN WS-PAR-PROF-BSC                                 ELXPMCCP
01601                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01602                 SET PMCI-PAR-BSC TO TRUE                          ELXPMCCP
01603              WHEN WS-PAR-PROF-MM                                  ELXPMCCP
01604                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01605                 SET PMCI-PAR-MM TO TRUE                           ELXPMCCP
01606              WHEN WS-PAR-PROF-BOTH                                ELXPMCCP
01607                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01608                 SET PMCI-PAR-BSC-MM TO TRUE                       ELXPMCCP
01609              WHEN WS-PAR-CALL                                     ELXPMCCP
01610                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01611              WHEN OTHER                                           ELXPMCCP
01612                 SET PRE-CERT-APPLIES TO TRUE                      ELXPMCCP
01613           END-EVALUATE                                            ELXPMCCP
01614      END-IF.                                                      ELXPMCCP
01615                                                                   ELXPMCCP
01616 ************************************************************      ELXPMCCP
01617  8000-TAG-WEEKEND-ADMISSION.                                      ELXPMCCP
01618      MOVE GCG-FRI-SAT-ADM-IND TO WS-COMMON-PART-IND.              ELXPMCCP
01619                                                                   ELXPMCCP
01620      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
01621           EVALUATE TRUE                                           ELXPMCCP
01622              WHEN WS-COMMON-INST-NO                               ELXPMCCP
01623                 SET WEEKEND-ADMIN-NO TO TRUE                      ELXPMCCP
01624              WHEN WS-COMMON-INST-BSC                              ELXPMCCP
01625                 SET WEEKEND-ADMIN-YES TO TRUE                     ELXPMCCP
01626                 SET PMCI-WKND-ADMSN-BSC TO TRUE                   ELXPMCCP
01627              WHEN WS-COMMON-INST-MM                               ELXPMCCP
01628                 SET WEEKEND-ADMIN-YES TO TRUE                     ELXPMCCP
01629                 SET PMCI-WKND-ADMSN-MM TO TRUE                    ELXPMCCP
01630              WHEN WS-COMMON-INST-BOTH                             ELXPMCCP
01631                 SET WEEKEND-ADMIN-YES TO TRUE                     ELXPMCCP
01632                 SET PMCI-WKND-ADMSN-BSC-MM TO TRUE                ELXPMCCP
01633              WHEN WS-COMMON-CALL                                  ELXPMCCP
01634                 SET WEEKEND-ADMIN-CALL TO TRUE                    ELXPMCCP
01635              WHEN OTHER                                           ELXPMCCP
01636                 SET WEEKEND-ADMIN-CALL TO TRUE                    ELXPMCCP
01637           END-EVALUATE                                            ELXPMCCP
01638      ELSE                                                         ELXPMCCP
01639           EVALUATE TRUE                                           ELXPMCCP
01640              WHEN WS-COMMON-PROF-NO                               ELXPMCCP
01641                 SET WEEKEND-ADMIN-NO TO TRUE                      ELXPMCCP
01642              WHEN WS-COMMON-PROF-BSC                              ELXPMCCP
01643                 SET WEEKEND-ADMIN-YES TO TRUE                     ELXPMCCP
01644                 SET PMCI-WKND-ADMSN-BSC TO TRUE                   ELXPMCCP
01645              WHEN WS-COMMON-PROF-MM                               ELXPMCCP
01646                 SET WEEKEND-ADMIN-YES TO TRUE                     ELXPMCCP
01647                 SET PMCI-WKND-ADMSN-MM TO TRUE                    ELXPMCCP
01648              WHEN WS-COMMON-PROF-BOTH                             ELXPMCCP
01649                 SET WEEKEND-ADMIN-YES TO TRUE                     ELXPMCCP
01650                 SET PMCI-WKND-ADMSN-BSC-MM TO TRUE                ELXPMCCP
01651              WHEN WS-COMMON-CALL                                  ELXPMCCP
01652                 SET WEEKEND-ADMIN-CALL TO TRUE                    ELXPMCCP
01653              WHEN OTHER                                           ELXPMCCP
01654                 SET WEEKEND-ADMIN-CALL TO TRUE                    ELXPMCCP
01655           END-EVALUATE                                            ELXPMCCP
01656      END-IF.                                                      ELXPMCCP
01657 ****************************************************************  ELXPMCCP
01658 * 8200-TAG-BAE-PROGRAM..                                       *  ELXPMCCP
01659 ****************************************************************  ELXPMCCP
01660  8200-TAG-BAE-PROGRAM.                                            ELXPMCCP
01661      MOVE GCG-BAE-INDICATOR TO WS-BAE-PART-IND.                   ELXPMCCP
01662                                                                   ELXPMCCP
01663      IF PMCI-PROFESSIONAL                                         ELXPMCCP
01664           EVALUATE TRUE                                           ELXPMCCP
01665              WHEN WS-BAE-PROF-NO                                  ELXPMCCP
01666                 SET PMCI-PRG-NO TO TRUE                           ELXPMCCP
01667              WHEN WS-BAE-PROF-YES                                 ELXPMCCP
01668                 SET PMCI-PRG-BAE-APPLIES TO TRUE                  ELXPMCCP
01669                 PERFORM 6800-DETERMINE-LOB-PRV                    ELXPMCCP
01670                 PERFORM 8250-EVAL-PROF-BAE-PROV                   ELXPMCCP
01671              WHEN OTHER                                           ELXPMCCP
01672                 SET PMCI-PRG-CALL TO TRUE                         ELXPMCCP
01673                 SET PMCI-PRV-CALL TO TRUE                         ELXPMCCP
01674           END-EVALUATE                                            ELXPMCCP
01675      ELSE                                                         ELXPMCCP
01676      IF PMCI-INSTITUTIONAL                                        ELXPMCCP
01677           CONTINUE.                                               ELXPMCCP
01678 ****************************************************************  ELXPMCCP
01679 * 8250-EVAL-PROF-BAE-PROV                                      *  ELXPMCCP
01680 ****************************************************************  ELXPMCCP
01681  8250-EVAL-PROF-BAE-PROV.                                         ELXPMCCP
01682      SET WS-NO-TAB TO TRUE.                                       ELXPMCCP
01683      EVALUATE TRUE                                                ELXPMCCP
01684         WHEN WS-BAE-PROF-STD                                      ELXPMCCP
01685 *          IF PMCI-BAE-PROVIDER                                   ELXPMCCP
01686            IF PMCI-PPO-PROVIDER                                   ELXPMCCP
01687               SET PMCI-PRV-BAE-IN TO TRUE                         ELXPMCCP
01688            ELSE                                                   ELXPMCCP
01689               SET PMCI-PRV-BAE-OUT TO TRUE                        ELXPMCCP
01690            END-IF                                                 ELXPMCCP
01691         WHEN WS-BAE-PROF-NON-STD                                  ELXPMCCP
01692            PERFORM 8460-DETER-BAE-NON-STD                         ELXPMCCP
01693      END-EVALUATE.                                                ELXPMCCP
01694 *    IF PMCI-PRV-BAE-OUT AND                                      ELXPMCCP
01695 *             WS-BAE-PROF-NON-STD                                 ELXPMCCP
01696 *       PERFORM 8470-TEST-BAE-IN-PPO.                             ELXPMCCP
01697                                                                   ELXPMCCP
01698 ****************************************************************  ELXPMCCP
01699 * 8460-DETER-BAE-NON-STD                                       *  ELXPMCCP
01700 * WHEN PROVIDER AREA ADDED BAE SUPPORT TO PMCI ADD BAE TYPE    *  ELXPMCCP
01701 ****************************************************************  ELXPMCCP
01702  8460-DETER-BAE-NON-STD.                                          ELXPMCCP
01703      MOVE '#GBAE ' TO WS-TABULAR.                                 ELXPMCCP
01704      PERFORM 8500-DETER-TAB-SEARCH.                               ELXPMCCP
01705      EVALUATE TRUE                                                ELXPMCCP
01706       WHEN WS-NO-TAB                                              ELXPMCCP
01707 *       IF PMCI-BAE-PROVIDER                                      ELXPMCCP
01708         IF PMCI-PPO-PROVIDER                                      ELXPMCCP
01709            SET PMCI-PRV-BAE-IN TO TRUE                            ELXPMCCP
01710         ELSE                                                      ELXPMCCP
01711 *          IF PMCI-NON-BAE-PROVIDER                               ELXPMCCP
01712            IF PMCI-NON-PPO-PROVIDER                               ELXPMCCP
01713               SET PMCI-PRV-BAE-OUT TO TRUE                        ELXPMCCP
01714            ELSE                                                   ELXPMCCP
01715               SET PMCI-BC-INVALID-DATA TO TRUE                    ELXPMCCP
01716               MOVE +1014 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCCP
01717            END-IF                                                 ELXPMCCP
01718         END-IF                                                    ELXPMCCP
01719      WHEN WS-NO-MATCH-TAB                                         ELXPMCCP
01720         IF GS16-CORP-LIST-OVERIDE-IND = 'I'                       ELXPMCCP
01721            SET PMCI-PRV-BAE-OUT TO TRUE                           ELXPMCCP
01722         ELSE                                                      ELXPMCCP
01723 *          IF PMCI-BAE-PROVIDER                                   ELXPMCCP
01724            IF PMCI-PPO-PROVIDER                                   ELXPMCCP
01725               SET PMCI-PRV-BAE-IN TO TRUE                         ELXPMCCP
01726            ELSE                                                   ELXPMCCP
01727 *             IF PMCI-NON-BAE-PROVIDER                            ELXPMCCP
01728               IF PMCI-NON-PPO-PROVIDER                            ELXPMCCP
01729                  SET PMCI-PRV-BAE-OUT TO TRUE                     ELXPMCCP
01730               ELSE                                                ELXPMCCP
01731                  SET PMCI-BC-INVALID-DATA TO TRUE                 ELXPMCCP
01732                  MOVE +1014 TO PMCI-BLUE-CHIP-ERROR-CODE          ELXPMCCP
01733               END-IF                                              ELXPMCCP
01734            END-IF                                                 ELXPMCCP
01735         END-IF                                                    ELXPMCCP
01736      WHEN WS-TAB-INCLUDE                                          ELXPMCCP
01737         SET PMCI-PRV-BAE-IN TO TRUE                               ELXPMCCP
01738      WHEN WS-TAB-EXCLUDE                                          ELXPMCCP
01739         SET PMCI-PRV-BAE-OUT TO TRUE                              ELXPMCCP
01740      WHEN OTHER                                                   ELXPMCCP
01741         MOVE +1015 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCCP
01742         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCCP
01743      END-EVALUATE.                                                ELXPMCCP
01744                                                                   ELXPMCCP
01745                                                                   ELXPMCCP
01746 ****************************************************************  ELXPMCCP
01747 * 8500-DETER-TAB-SEARCH.                                       *  ELXPMCCP
01748 ****************************************************************  ELXPMCCP
01749  8500-DETER-TAB-SEARCH.                                           ELXPMCCP
01750      INITIALIZE KWA-PROVISION-SLOT-NO.                            ELXPMCCP
01751      SET WS-NO-TAB-FOUND TO TRUE.                                 ELXPMCCP
01752      PERFORM VARYING GCG-INDEX FROM 1                             ELXPMCCP
01753             BY 1 UNTIL WS-TAB-FOUND                               ELXPMCCP
01754               OR (GCG-INDEX >                                     ELXPMCCP
01755                  GCG-COUNT-TAB-PROVN-POINTERS)                    ELXPMCCP
01756        IF GCG-TAB-ID (GCG-INDEX) = WS-TABULAR                     ELXPMCCP
01757           MOVE GCG-TAB-ID (GCG-INDEX)                             ELXPMCCP
01758              TO KWA-PROVISION-ID                                  ELXPMCCP
01759           MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                        ELXPMCCP
01760              TO KWA-PROVISION-SLOT-NO                             ELXPMCCP
01761        END-IF                                                     ELXPMCCP
01762      END-PERFORM.                                                 ELXPMCCP
01763      IF KWA-PROVISION-SLOT-NO = ZEROES                            ELXPMCCP
01764         CONTINUE                                                  ELXPMCCP
01765      ELSE                                                         ELXPMCCP
01766         SET WS-TAB-FOUND TO TRUE                                  ELXPMCCP
01767         PERFORM 8550-GET-TABLR.                                   ELXPMCCP
01768                                                                   ELXPMCCP
01769 ****************************************************************  ELXPMCCP
01770 * 8550-GET-TABLR.                                              *  ELXPMCCP
01771 * UDPATED, RGO, 4/96                                           *  ELXPMCCP
01772 ****************************************************************  ELXPMCCP
01773  8550-GET-TABLR.                                                  ELXPMCCP
01774      SET CIA-GCTABULR-DDN TO TRUE.                                ELXPMCCP
01775      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCCP
01776         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                   ELXPMCCP
01777      IF CIA-RC-PTR-NULL                                           ELXPMCCP
01778         SET CIA-STG-GETMAIN TO TRUE                               ELXPMCCP
01779         CALL 'ELUSTGMG' USING DFHEIBLK                            ELXPMCCP
01780                         DFHCOMMAREA                               ELXPMCCP
01781         SET CIA-GCTABULR-DDN TO TRUE                              ELXPMCCP
01782         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCCP
01783                         ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.   ELXPMCCP
01784      SET IOP-RD TO TRUE.                                          ELXPMCCP
01785      SET IOP-STG-MODE-MOVE TO TRUE.                               ELXPMCCP
01786      SET IOP-FCQ-NONE TO TRUE.                                    ELXPMCCP
01787      SET IOP-KVQ-EQ TO TRUE.                                      ELXPMCCP
01788      MOVE KWA-GCTABULR-KEY TO IOP-FILE-KEY.                       ELXPMCCP
01789      CALL 'ELUIOPGM' USING DFHEIBLK                               ELXPMCCP
01790                            DFHCOMMAREA.                           ELXPMCCP
01791      IF IOP-RC-OK                                                 ELXPMCCP
01792         IF PMCI-PRG-PPO-APPLIES                                   ELXPMCCP
01793            SET ADDRESS OF GPPO-TBLR-LIST TO IOP-REC-PTR           ELXPMCCP
01794            PERFORM 8600-SEARCH-TAB-FOR-PROV                       ELXPMCCP
01795         ELSE                                                      ELXPMCCP
01796            IF PMCI-PRG-RPO-APPLIES                                ELXPMCCP
01797                SET ADDRESS OF GRPO-TBLR-LIST TO IOP-REC-PTR       ELXPMCCP
01798                PERFORM 8700-SEARCH-TAB-FOR-PROV-RPO               ELXPMCCP
01799         ELSE                                                      ELXPMCCP
01800            IF PMCI-PRG-CBL-APPLIES                                ELXPMCCP
01801                SET ADDRESS OF GCBL-TBLR-LIST TO IOP-REC-PTR       ELXPMCCP
01802                PERFORM 8800-SEARCH-TAB-FOR-PROV-CBL               ELXPMCCP
01803         ELSE                                                      ELXPMCCP
01804            IF PMCI-PRG-CPO-APPLIES                                ELXPMCCP
01805                SET ADDRESS OF GCPO-TBLR-LIST TO IOP-REC-PTR       ELXPMCCP
01806                PERFORM 8900-SEARCH-TAB-FOR-PROV-CPO               ELXPMCCP
01807         ELSE                                                      ELXPMCCP
01808            IF PMCI-PRG-BAE-APPLIES                                ELXPMCCP
01809                SET ADDRESS OF GBAE-TBLR-LIST TO IOP-REC-PTR       ELXPMCCP
01810                PERFORM 8900-SEARCH-TAB-FOR-PROV-BAE               ELXPMCCP
01811      ELSE                                                         ELXPMCCP
01812         SET SW-TRMNL-ERR TO TRUE                                  ELXPMCCP
01813         MOVE +1002 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCCP
01814         SET PMCI-BC-INTERNAL-ERROR TO TRUE                        ELXPMCCP
01815      END-IF.                                                      ELXPMCCP
01816                                                                   ELXPMCCP
01817  8600-SEARCH-TAB-FOR-PROV.                                        ELXPMCCP
01818      SET WS-NO-MATCH-TAB TO TRUE.                                 ELXPMCCP
01819      PERFORM VARYING GSW-INDEX FROM 1 BY 1                        ELXPMCCP
01820         UNTIL GSW-PROVIDER-NO-ARG (GSW-INDEX)                     ELXPMCCP
01821           > PMCI-PROVIDER-NUMBER OR (GSW-INDEX >                  ELXPMCCP
01822           GSW-ENTRY-COUNT)                                        ELXPMCCP
01823           IF GSW-PROVIDER-NO-ARG (GSW-INDEX) =                    ELXPMCCP
01824             PMCI-PROVIDER-NUMBER                                  ELXPMCCP
01825             IF GSW-PROVIDER-INCLUDED (GSW-INDEX)                  ELXPMCCP
01826                SET WS-TAB-INCLUDE TO TRUE                         ELXPMCCP
01827             ELSE                                                  ELXPMCCP
01828                SET WS-TAB-EXCLUDE TO TRUE                         ELXPMCCP
01829             END-IF                                                ELXPMCCP
01830           END-IF                                                  ELXPMCCP
01831      END-PERFORM.                                                 ELXPMCCP
01832                                                                   ELXPMCCP
01833  8700-SEARCH-TAB-FOR-PROV-RPO.                                    ELXPMCCP
01834      SET WS-NO-MATCH-TAB TO TRUE.                                 ELXPMCCP
01835      PERFORM VARYING GS7-INDEX FROM 1 BY 1                        ELXPMCCP
01836         UNTIL GS7-PROVIDER-NO-ARG (GS7-INDEX)                     ELXPMCCP
01837           > PMCI-PROVIDER-NUMBER OR (GS7-INDEX >                  ELXPMCCP
01838           GS7-ENTRY-COUNT)                                        ELXPMCCP
01839           IF GS7-PROVIDER-NO-ARG (GS7-INDEX) =                    ELXPMCCP
01840             PMCI-PROVIDER-NUMBER                                  ELXPMCCP
01841             IF GS7-PROVIDER-INCLUDED (GS7-INDEX)                  ELXPMCCP
01842                SET WS-TAB-INCLUDE TO TRUE                         ELXPMCCP
01843             ELSE                                                  ELXPMCCP
01844                SET WS-TAB-EXCLUDE TO TRUE                         ELXPMCCP
01845             END-IF                                                ELXPMCCP
01846           END-IF                                                  ELXPMCCP
01847      END-PERFORM.                                                 ELXPMCCP
01848 ****************************************************************  ELXPMCCP
01849 * 8800-.  CREATED, 4/96 RGO                                    *  ELXPMCCP
01850 ****************************************************************  ELXPMCCP
01851  8800-SEARCH-TAB-FOR-PROV-CBL.                                    ELXPMCCP
01852      SET WS-NO-MATCH-TAB TO TRUE.                                 ELXPMCCP
01853      PERFORM VARYING GS9-INDEX FROM 1 BY 1                        ELXPMCCP
01854         UNTIL GS9-PROVIDER-NO-ARG (GS9-INDEX)                     ELXPMCCP
01855           > PMCI-PROVIDER-NUMBER OR (GS9-INDEX >                  ELXPMCCP
01856           GS9-ENTRY-COUNT)                                        ELXPMCCP
01857           IF GS9-PROVIDER-NO-ARG (GS9-INDEX) =                    ELXPMCCP
01858             PMCI-PROVIDER-NUMBER                                  ELXPMCCP
01859             IF GS9-PROVIDER-INCLUDED (GS9-INDEX)                  ELXPMCCP
01860                SET WS-TAB-INCLUDE TO TRUE                         ELXPMCCP
01861             ELSE                                                  ELXPMCCP
01862                SET WS-TAB-EXCLUDE TO TRUE                         ELXPMCCP
01863             END-IF                                                ELXPMCCP
01864           END-IF                                                  ELXPMCCP
01865      END-PERFORM.                                                 ELXPMCCP
01866                                                                   ELXPMCCP
01867 ****************************************************************  ELXPMCCP
01868 * 8900-SEARCH-TAB-FOR-PROV-BAE                                 *  ELXPMCCP
01869 ****************************************************************  ELXPMCCP
01870  8900-SEARCH-TAB-FOR-PROV-BAE.                                    ELXPMCCP
01871      SET WS-NO-MATCH-TAB TO TRUE.                                 ELXPMCCP
01872      PERFORM VARYING GS16-INDEX FROM 1 BY 1                       ELXPMCCP
01873         UNTIL GS16-PROVIDER-NO-ARG (GS16-INDEX)                   ELXPMCCP
01874           > PMCI-PROVIDER-NUMBER OR (GS16-INDEX >                 ELXPMCCP
01875           GS16-ENTRY-COUNT)                                       ELXPMCCP
01876           IF GS16-PROVIDER-NO-ARG (GS16-INDEX) =                  ELXPMCCP
01877             PMCI-PROVIDER-NUMBER                                  ELXPMCCP
01878             IF GS16-PROVIDER-INCLUDED (GS16-INDEX)                ELXPMCCP
01879                SET WS-TAB-INCLUDE TO TRUE                         ELXPMCCP
01880             ELSE                                                  ELXPMCCP
01881                SET WS-TAB-EXCLUDE TO TRUE                         ELXPMCCP
01882             END-IF                                                ELXPMCCP
01883           END-IF                                                  ELXPMCCP
01884      END-PERFORM.                                                 ELXPMCCP
01885 ****************************************************************  ELXPMCCP
01886 * 8900-SEARCH-TAB-FOR-PROV-CPO                                 *  ELXPMCCP
01887 ****************************************************************  ELXPMCCP
01888  8900-SEARCH-TAB-FOR-PROV-CPO.                                    ELXPMCCP
01889      SET WS-NO-MATCH-TAB TO TRUE.                                 ELXPMCCP
01890      PERFORM VARYING GS8-INDEX FROM 1 BY 1                        ELXPMCCP
01891         UNTIL GS8-PROVIDER-NO-ARG (GS8-INDEX)                     ELXPMCCP
01892           > PMCI-PROVIDER-NUMBER OR (GS8-INDEX >                  ELXPMCCP
01893           GS8-ENTRY-COUNT)                                        ELXPMCCP
01894           IF GS8-PROVIDER-NO-ARG (GS8-INDEX) =                    ELXPMCCP
01895             PMCI-PROVIDER-NUMBER                                  ELXPMCCP
01896             IF GS8-PROVIDER-INCLUDED (GS8-INDEX)                  ELXPMCCP
01897                SET WS-TAB-INCLUDE TO TRUE                         ELXPMCCP
01898             ELSE                                                  ELXPMCCP
01899                SET WS-TAB-EXCLUDE TO TRUE                         ELXPMCCP
01900             END-IF                                                ELXPMCCP
01901           END-IF                                                  ELXPMCCP
01902      END-PERFORM.                                                 ELXPMCCP
01903 ************************************************************      ELXPMCCP
01904 *9000-CALL-TO-I-O-PGM.                                     *      ELXPMCCP
01905 ************************************************************      ELXPMCCP
01906  9000-CALL-I-O-PGM.                                               ELXPMCCP
01907      CALL 'ELUIOPGM' USING                                        ELXPMCCP
01908           DFHEIBLK DFHCOMMAREA                                    ELXPMCCP
01909      END-CALL.                                                    ELXPMCCP
01910      IF IOP-RC-OK                                                 ELXPMCCP
01911         SET ADDRESS OF GCCP-TBLR-LIST TO IOP-REC-PTR              ELXPMCCP
01912         SET IOP-REC-PTR TO NULL                                   ELXPMCCP
01913      ELSE                                                         ELXPMCCP
01914         IF IOP-RC-NOTFND                                          ELXPMCCP
01915            MOVE +1009 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCCP
01916            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCCP
01917         ELSE                                                      ELXPMCCP
01918            MOVE +1010 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCCP
01919            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCCP
01920         END-IF                                                    ELXPMCCP
01921      END-IF.                                                      ELXPMCCP
01922                                                                   ELXPMCCP
01923 ************************************************************      ELXPMCCP
01924 *9100-CALL-STRG-MNGR.                                      *      ELXPMCCP
01925 ************************************************************      ELXPMCCP
01926  9100-CALL-STRG-MNGR.                                             ELXPMCCP
01927      MOVE ZERO TO CIA-AREA-LEN.                                   ELXPMCCP
01928      SET CIA-STG-GETMAIN TO TRUE.                                 ELXPMCCP
01929      CALL 'ELUSTGMG' USING                                        ELXPMCCP
01930             DFHEIBLK                                              ELXPMCCP
01931             DFHCOMMAREA                                           ELXPMCCP
01932      END-CALL.                                                    ELXPMCCP
