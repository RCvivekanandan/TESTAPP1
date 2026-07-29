00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGAOL  
00003  PROGRAM-ID.         ELGAOL.                                         LV002
00004                                                                   ELGAOL  
00005  AUTHOR.             LUCY TORRES.                                 ELGAOL  
00006                                                                   ELGAOL  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGAOL  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGAOL  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGAOL  
00010                      233 N. MICHIGAN AVE                          ELGAOL  
00011                      CHICAGO, ILLINOIS 60601                      ELGAOL  
00012                                                                   ELGAOL  
00013  DATE-WRITTEN.       08-JUL-1987.                                 ELGAOL  
00014                                                                   ELGAOL  
00015  DATE-COMPILED.                                                   ELGAOL  
00016                                                                   ELGAOL  
00017  SECURITY.           COPYRIGHT 1986,                              ELGAOL  
00018                      HEALTH CARE SERVICE CORPORATION              ELGAOL  
00019      SKIP3                                                        ELGAOL  
00020                                                                   ELGAOL  
00021  ENVIRONMENT DIVISION.                                            ELGAOL  
00022                                                                   ELGAOL  
00023  CONFIGURATION SECTION.                                           ELGAOL  
00024  SOURCE-COMPUTER.    IBM-3090.                                    ELGAOL  
00025  OBJECT-COMPUTER.    IBM-3090.                                    ELGAOL  
00026      EJECT                                                        ELGAOL  
00027 ******************************************************************ELGAOL  
00028 *                                                                *ELGAOL  
00029 *  ELGAOL   - ELS:  GENERATES THE OUTPUT FOR OUT OF POCKET AT THE*ELGAOL  
00030 *                   TOPIC, BENEFIT PROVISION, AND COST           *ELGAOL  
00031 *                   CONTAINMENT LEVELS.                          *ELGAOL  
00032 *                                                                *ELGAOL  
00033 ******************************************************************ELGAOL  
00034 *                                                                *ELGAOL  
00035 *                      MAINTENANCE HISTORY                       *ELGAOL  
00036 *                                                                *ELGAOL  
00037 *  MOD     DATE     BY  DRPT                ACTION               *ELGAOL  
00038 * ----- ----------- --- ----- ---------------------------------- *ELGAOL  
00039 * 01.00 08-JUL-1987 LET       CREATED                            *ELGAOL  
00040 *                                                                *ELGAOL  
00041 * 01.01 27-AUG-1987 LET       SEPERATED THE COST CONTAINMENT     *ELGAOL  
00042 *                             LEVEL PROCESSING FROM THE OTHER    *ELGAOL  
00043 *                             LEVELS OF PROCESSING.              *ELGAOL  
00044 *                                                                *ELGAOL  
00045 * 01.02 30-SEP-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELGAOL  
00046 *                             VERSION OF COPYBOOK ELSACCUM.      *ELGAOL  
00047 *                                                                *ELGAOL  
00048 * 01.03 01-OCT-1987 REB       ADDED LOGIC TO HANDLE PROCESSING   *ELGAOL  
00049 *                             FOR GROUP/CONTRACT ACCUMS.         *ELGAOL  
00050 *                                                                *ELGAOL  
00051 * 01.04 09-OCT-1987 LET       DELETED LOGIC FOR GROUP/CONTRACT   *ELGAOL  
00052 *                             ACCUMS.                            *ELGAOL  
00053 *                                                                *ELGAOL  
00054 * 01.05 09-NOV-1987 REB       ADDED LOGIC TO HANDLE MULTIPLE     *ELGAOL  
00055 *                             INTERNALS FOR ASCEND/DESCEND IF    *ELGAOL  
00056 *                             THEY EXIST.                        *ELGAOL  
00057 *                                                                *ELGAOL  
00058 * 01.06 10-NOV-1987 REB       CORRECTED CONDITIONALS IN THE      *ELGAOL  
00059 *                             APPLICABILITY PHRASE.              *ELGAOL  
00060 *                                                                *ELGAOL  
00061 * 01.07 13-NOV-1987 REB       CHANGED CODE TO ASSOCIATE THE      *ELGAOL  
00062 *                             CORRECT PERCENT LEVEL WITH ACTUAL  *ELGAOL  
00063 *                             INTERNAL TABULAR.                  *ELGAOL  
00064 *                                                                *ELGAOL  
00065 * 01.08 19-NOV-1987 REB       USED PERCENT LEVEL PASSED BY SETUP *ELGAOL  
00066 *                             FOR INTERNAL TABULARS WITH ASC/DESC*ELGAOL  
00067 *                                                                *ELGAOL  
00068 * 01.08 03-JAN-1991 AKK       STORAGE MANAGEMENT ENHANCEMENTS    *ELGAOL  
00069 *                                                                *ELGAOL  
00070 * 01.09 08-FEB-1991 AKK       ADD SUPPORT FOR IPGP AND IDGD INT- *ELGAOL  
00071 *                             TERNAL TABULARS.                   *ELGAOL  
00072 *                                                                *ELGAOL  
00073 * 01.10 08-NOV-1991 JPB       ENHANCED DISPLAY OF ACCUMULATION   *ELGAOL  
00074 *                             LEVELS FOR INTERNALS.              *ELGAOL  
00075 *                                                                *ELGAOL  
00076 * 01.11 23-MAR-1992 JPB       CLONED FROM ELGACL FOR ACCUM       *ELGAOL  
00077 *                             DISPLAY PROJECT.                   *ELGAOL  
00078 *                                                                *ELGAOL  
00079 * 01.12 31-MAR-1992 JPB       ADDED MISSING PERIOD TO APPLIC-PHR *ELGAOL  
00080 *                             TO FIX DISCREPANCY.                *ELGAOL  
00081 *                                                                *ELGAOL  
00082 * 01.13 25-AUG-2000 AKK       ADD SUPPORT FOR #IPGS TABUALR      *ELGAOL  
00083 *                                                                *ELGAOL  
00084 * 01.14 08-JAN-2003 AKK       REGEN'D DUE TO ADDN OF SMI AND NSM *ELGAOL  
00085 *                                                                *ELGAOL  
00086 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGAOL  
00087 *                                                                *ELGAOL  
ED0624* BBDA-58217 06/14/24  ED     RECOMPILE FOR PEAQ COPYBOOK        *        
ED0624*                             EXPANSION:                         *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00088 ******************************************************************ELGAOL  
00089                                                                   ELGAOL  
00090  DATA DIVISION.                                                   ELGAOL  
00091  WORKING-STORAGE SECTION.                                         ELGAOL  
00092  77  PAN-VALET PICTURE X(24) VALUE '004ELGAOLTS  03/08/91'.       ELGAOL  
00093  77  PAN-DSN   PICTURE  X(44) VALUE                               ELGAOL  
00094      'HCMSGEN.TEST.PANLIB                         '.              ELGAOL  
00095 /                                                                 ELGAOL  
00096  01  HEADERS.                                                     ELGAOL  
00097      05  TOPIC-HEADER .                                           ELGAOL  
00098          10  FILLER                  PIC  X(34) VALUE SPACES.     ELGAOL  
00099          10  FILLER                  PIC  X(13) VALUE             ELGAOL  
00100                  'OUT OF POCKET'.                                 ELGAOL  
00101          10  FILLER                  PIC  X(34) VALUE SPACES.     ELGAOL  
00102      05  BENEFIT-PROVISION-HEADER.                                ELGAOL  
00103          10  FILLER                  PIC  X(31) VALUE             ELGAOL  
00104                  'OUT OF POCKET AT BENEFIT LEVEL:'.               ELGAOL  
00105          10  FILLER                  PIC  X(50) VALUE SPACES.     ELGAOL  
00106                                                                   ELGAOL  
00107  01  MESSAGES.                                                    ELGAOL  
00108      05  CARRY-OVER-MSG              PIC  X(47) VALUE             ELGAOL  
00109         'THE OUT-OF-POCKET EXPENSE CARRY OVER CREDIT IS '.        ELGAOL  
00110      05  OTHER-SOURCE-PHRASE         PIC  X(31) VALUE             ELGAOL  
00111              ' UP TO AN AMOUNT DETERMINED BY '.                   ELGAOL  
00112      05  OUT-OF-POCKET-END-MSG-1A    PIC  X(81) VALUE             ELGAOL  
00113              'ADDITIONAL OUT-OF-POCKET MAY APPLY TO INDIVIDUAL BENELGAOL  
00114 -            'EFITS.  SEE SPECIFIC TOPICS  '.                     ELGAOL  
00115      05  OUT-OF-POCKET-END-MSG-1B    PIC  X(29) VALUE             ELGAOL  
00116              'FOR ADDITIONAL OUT-OF-POCKET.'.                     ELGAOL  
00117      05  NO-ACCUMS-MSG               PIC  X(49) VALUE             ELGAOL  
00118              'NO GROUP OR CONTRACT LEVEL OUT-OF-POCKET APPLIES.'. ELGAOL  
00119      05  NO-INST-LOB-MSG             PIC  X(63) VALUE             ELGAOL  
00120              'NO INSTITUTIONAL GROUP OR CONTRACT LEVEL OUT-OF-POCKELGAOL  
00121 -            'ET APPLIES.'.                                       ELGAOL  
00122      05  NO-PROF-LOB-MSG             PIC  X(62) VALUE             ELGAOL  
00123              'NO PROFESSIONAL GROUP OR CONTRACT LEVEL OUT-OF-POCKEELGAOL  
00124 -            'T APPLIES.'.                                        ELGAOL  
00125                                                                   ELGAOL  
00126  01  PROGRAM-CONSTANTS.                                           ELGAOL  
00127      05  PC-AOL                      PIC  X(06) VALUE             ELGAOL  
00128              '#AOL  '.                                            ELGAOL  
00129      05  PC-AND                      PIC  X(04) VALUE             ELGAOL  
00130              ' AND'.                                              ELGAOL  
00131      05  PC-AND-APPLIES-TO           PIC  X(16) VALUE             ELGAOL  
00132              ' AND APPLIES TO '.                                  ELGAOL  
00133      05  PC-AND-IS                   PIC  X(08) VALUE             ELGAOL  
00134              ' AND IS '.                                          ELGAOL  
00135      05  PC-ANOTHER-SOURCE           PIC  X(15) VALUE             ELGAOL  
00136              'ANOTHER SOURCE.'.                                   ELGAOL  
00137      05  PC-APPLIES                  PIC  X(09) VALUE             ELGAOL  
00138              ' APPLIES '.                                         ELGAOL  
00139      05  PC-CHANGES-TO               PIC  X(16) VALUE             ELGAOL  
00140              'THEN CHANGES TO '.                                  ELGAOL  
00141      05  PC-OUT-OF-POCKET-APPLIES    PIC  X(26) VALUE             ELGAOL  
00142              ' OUT-OF-POCKET APPLIES TO '.                        ELGAOL  
00143      05  PC-CONSIDERATIONS           PIC  X(29) VALUE             ELGAOL  
00144              ' CONSIDERATIONS LISTED BELOW '.                     ELGAOL  
00145      05  PC-DOLLARS                  PIC  X(07) VALUE             ELGAOL  
00146              'DOLLARS'.                                           ELGAOL  
00147      05  PC-DOTS                     PIC  X(59) VALUE             ELGAOL  
00148              '....................................................ELGAOL  
00149 -            '.......'.                                           ELGAOL  
00150      05  PC-FOR                      PIC  X(05) VALUE             ELGAOL  
00151              ' FOR '.                                             ELGAOL  
00152      05  PC-IBGR                     PIC  X(06) VALUE '#IBGR '.   ELGAOL  
00153      05  PC-IBGR-NAME                PIC  X(18) VALUE             ELGAOL  
00154              ' BENEFIT PROVISION'.                                ELGAOL  
00155      05  PC-IPGN                     PIC  X(06) VALUE '#IPGN '.   ELGAOL  
00156      05  PC-IPGN-NAME                PIC  X(18) VALUE             ELGAOL  
00157              '   PROVIDER NUMBER'.                                ELGAOL  
00158      05  PC-IPGT                     PIC  X(06) VALUE '#IPGT '.   ELGAOL  
00159      05  PC-IPGT-NAME                PIC  X(18) VALUE             ELGAOL  
00160              '     PROVIDER TYPE'.                                ELGAOL  
00161      05  PC-IPGP                     PIC  X(06) VALUE '#IPGP '.   ELGAOL  
00162      05  PC-IPGP-NAME                PIC  X(18) VALUE             ELGAOL  
00163              '         PROCEDURE'.                                ELGAOL  
00164      05  PC-IDGD                     PIC  X(06) VALUE '#IDGD '.   ELGAOL  
00165      05  PC-IDGD-NAME                PIC  X(18) VALUE             ELGAOL  
00166              '         DIAGNOSIS'.                                ELGAOL  
00167      05  PC-IPGS                     PIC  X(06) VALUE '#IPGS '.   ELGAOL  
00168      05  PC-IPGS-NAME                PIC  X(18) VALUE             ELGAOL  
00169              'PROVIDER SPECIALTY'.                                ELGAOL  
00170      05  PC-MULTI-PCENT-START        PIC  X(21) VALUE             ELGAOL  
00171              'THE OUT-OF-POCKET IS:'.                             ELGAOL  
00172      05  PC-NOTE                     PIC  X(08) VALUE             ELGAOL  
00173              ' (NOTE: '.                                          ELGAOL  
00174      05  PC-OUTPKT-OTHR-SRCE-LEAD    PIC  X(19) VALUE             ELGAOL  
00175              'AN AMOUNT FOUND IN '.                               ELGAOL  
00176      05  PC-PER                      PIC  X(05) VALUE             ELGAOL  
00177              ' PER '.                                             ELGAOL  
00178      05  PC-PROVIDED                 PIC  X(10) VALUE             ELGAOL  
00179              ' PROVIDED '.                                        ELGAOL  
00180      05  PC-SERVICES                 PIC  X(10) VALUE             ELGAOL  
00181              ' SERVICES '.                                        ELGAOL  
00182      05  PC-SPACES                   PIC  X(16) VALUE             ELGAOL  
00183              '                '.                                  ELGAOL  
00184      05  PC-SUBJECT-TO               PIC  X(13) VALUE             ELGAOL  
00185              ', SUBJECT TO '.                                     ELGAOL  
00186      05  PC-THE                      PIC  X(04) VALUE             ELGAOL  
00187              'THE '.                                              ELGAOL  
00188      05  PC-THIS                     PIC  X(05) VALUE             ELGAOL  
00189              'THIS '.                                             ELGAOL  
00190      05  PC-TO                       PIC  X(04) VALUE             ELGAOL  
00191              ' TO '.                                              ELGAOL  
00192      05  PC-UNLIMITED                PIC  X(10) VALUE             ELGAOL  
00193              'UNLIMITED '.                                        ELGAOL  
00194                                                                   ELGAOL  
00195                                                                   ELGAOL  
00196  01  WS-DEFINITION-LINE.                                          ELGAOL  
00197      05  FILLER                   PIC  X(43) VALUE                ELGAOL  
00198              'THE OUT-OF-POCKET EXPENSE IS CALCULATED AS'.        ELGAOL  
00199                                                                   ELGAOL  
00200  01  WS-BEN-PER-PHRASE.                                           ELGAOL  
00201      05  FILLER                   PIC  X(36) VALUE                ELGAOL  
00202              'THE OUT-OF-POCKET LIMIT APPLIES PER '.              ELGAOL  
00203                                                                   ELGAOL  
00204  01  WS-BP-TIME-FCTR-QUAL-PHRASE.                                 ELGAOL  
00205      05 FILLER                    PIC  X(11) VALUE                ELGAOL  
00206              ' PERIOD OF '.                                       ELGAOL  
00207      05  WS-BP-TIME-FCTR          PIC  ZZ9.                       ELGAOL  
00208      05  WS-BP-TIME-FCTR-X REDEFINES                              ELGAOL  
00209            WS-BP-TIME-FCTR        PIC  X(03).                     ELGAOL  
00210      05  FILLER                   PIC  X(01) VALUE SPACE.         ELGAOL  
00211      05  WS-BP-TIME-QUAL          PIC  X(65).                     ELGAOL  
00212                                                                   ELGAOL  
00213  01  WS-INTERVAL-TIME-FCTR-QUAL-PHR.                              ELGAOL  
00214      05  FILLER                   PIC  X(14) VALUE                ELGAOL  
00215              ' SEPARATED BY '.                                    ELGAOL  
00216      05  WS-INTERVAL-TIME-FCTR    PIC  ZZ9.                       ELGAOL  
00217      05  WS-INTERVAL-TIME-FCTR-X REDEFINES                        ELGAOL  
00218            WS-INTERVAL-TIME-FCTR  PIC  X(03).                     ELGAOL  
00219      05  FILLER                   PIC  X(01) VALUE SPACE.         ELGAOL  
00220      05  WS-INTERVAL-TIME-QUAL    PIC  X(62).                     ELGAOL  
00221                                                                   ELGAOL  
00222  01  WS-INTERVAL-OVERRIDE-PHRASE-1.                               ELGAOL  
00223      05  FILLER                   PIC  X(33) VALUE                ELGAOL  
00224              'THE INTERVAL MAY BE OVERRULED IF '.                 ELGAOL  
00225      05  WS-INTERVAL-OVERRIDE-VALUE-1        PIC ZZZZ9.           ELGAOL  
00226      05  FILLER                   PIC  X(76) VALUE                ELGAOL  
00227          ' MONTHS HAVE ELAPSED FROM THE ADMISSION DATE OF THE FIRSELGAOL  
00228 -            'T COVERED ADMISSION.'.                              ELGAOL  
00229                                                                   ELGAOL  
00230  01  WS-INTERVAL-OVERRIDE-PHRASE-2.                               ELGAOL  
00231      05  FILLER                   PIC  X(73) VALUE                ELGAOL  
00232              'IF THE MEMBER IS MEDICARE ELIGIBLE, THE BENEFIT PERIELGAOL  
00233 -            'ODS ARE SEPARATED BY '.                             ELGAOL  
00234      05  WS-INTERVAL-OVERRIDE-VALUE-2        PIC ZZZZ9.           ELGAOL  
00235      05  FILLER                   PIC  X(06) VALUE ' DAYS.'.      ELGAOL  
00236                                                                   ELGAOL  
00237  01  WS-INTERVAL-OVERRIDE-PHRASE-3.                               ELGAOL  
00238      05  FILLER                   PIC  X(38) VALUE                ELGAOL  
00239              'THE INTERVAL CAN BE OVERRULED SO THAT '.            ELGAOL  
00240      05  WS-INTERVAL-OVERRIDE-VALUE-3        PIC ZZZZ9.           ELGAOL  
00241      05  FILLER                   PIC  X(53) VALUE                ELGAOL  
00242          ' DAYS/VISITS ARE PAID AT THE INDICATED PERCENT LEVEL.'. ELGAOL  
00243                                                                   ELGAOL  
00244  01  WS-FROM-AGE-QUAL-PHRASE.                                     ELGAOL  
00245      05  FILLER                   PIC  X(10) VALUE                ELGAOL  
00246              ' FROM AGE '.                                        ELGAOL  
00247      05  WS-FROM-AGE              PIC  ZZZZZZZZZ9.                ELGAOL  
00248      05  WS-FROM-AGE-ALPHA REDEFINES                              ELGAOL  
00249            WS-FROM-AGE            PIC  X(10).                     ELGAOL  
00250      05  FILLER                   PIC  X(01) VALUE SPACES.        ELGAOL  
00251      05  WS-FROM-AGE-QUAL         PIC  X(12).                     ELGAOL  
00252                                                                   ELGAOL  
00253  01  WS-TO-AGE-QUAL-PHRASE.                                       ELGAOL  
00254      05  FILLER                   PIC  X(10) VALUE                ELGAOL  
00255              ' TO AGE '.                                          ELGAOL  
00256      05  WS-TO-AGE                PIC  ZZZZZZZZZ9.                ELGAOL  
00257      05  WS-TO-AGE-ALPHA REDEFINES                                ELGAOL  
00258            WS-TO-AGE              PIC  X(10).                     ELGAOL  
00259      05  FILLER                   PIC  X(01) VALUE SPACES.        ELGAOL  
00260      05  WS-TO-AGE-QUAL           PIC  X(12).                     ELGAOL  
00261                                                                   ELGAOL  
00262  01  WS-VAL-LIMIT-AMOUNT.                                         ELGAOL  
00263      05  WS-VAL-LIM-DOLLARS       PIC  $$,$$$,$$9.99.             ELGAOL  
00264          88  WS-MAX-LIMIT-DOLLARS            VALUE                ELGAOL  
00265              '$9,999,999.99' '$9,999,999.00'.                     ELGAOL  
00266      05  FILLER                   PIC  X(34).                     ELGAOL  
00267                                                                   ELGAOL  
00268  01  WS-VAL-LIMIT-OTHER-AMT REDEFINES WS-VAL-LIMIT-AMOUNT.        ELGAOL  
00269      05  WS-VAL-LIM-OTHER         PIC  ZZZ,ZZZ,Z99.               ELGAOL  
00270          88  WS-MAX-LIMIT-OTHER              VALUE                ELGAOL  
00271              '999,999,999' '999,999,900'.                         ELGAOL  
00272      05  WS-VAL-QUAL-OTHER        PIC  X(36).                     ELGAOL  
00273                                                                   ELGAOL  
00274  01  WS-VAL-LIMIT-UNLIMITED REDEFINES WS-VAL-LIMIT-AMOUNT.        ELGAOL  
00275      05  WS-VAL-QUAL-UNLIMITED    PIC  X(47).                     ELGAOL  
00276                                                                   ELGAOL  
00277  01  WS-SINGLE-PERCENT-PHRASE.                                    ELGAOL  
00278      05  FILLER                   PIC  X(21) VALUE                ELGAOL  
00279              'THE OUT-OF-POCKET IS '.                             ELGAOL  
00280      05  WS-SINGLE-PERCENT-LEVEL  PIC ZZ9    VALUE ZERO.          ELGAOL  
00281      05  FILLER                   PIC  X(8)  VALUE                ELGAOL  
00282              '% UP TO '.                                          ELGAOL  
00283      05  WS-SINGLE-VAL-LMT-PHR    PIC  X(47).                     ELGAOL  
00284      05  FILLER                   PIC  X(01) VALUE SPACES.        ELGAOL  
00285                                                                   ELGAOL  
00286  01  WS-MULTI-VALUE-PHRASE.                                       ELGAOL  
00287      05  WS-MULTI-PERCENT-LEVEL   PIC  ZZ9   VALUE ZERO.          ELGAOL  
00288      05  FILLER                   PIC  X(08) VALUE                ELGAOL  
00289          '% UP TO '.                                              ELGAOL  
00290      05  WS-MULTI-VAL-LMT-PHR     PIC  X(47) VALUE SPACES.        ELGAOL  
00291                                                                   ELGAOL  
00292  01  WS-MULTI-VALUE-LINE.                                         ELGAOL  
00293      05  WS-MVL-CHANGES-PHRASE    PIC  X(16) VALUE SPACES.        ELGAOL  
00294      05  WS-MVL-MASK              PIC  X(58) VALUE SPACES.        ELGAOL  
00295      05  FILLER                   PIC  X(01) VALUE SPACES.        ELGAOL  
00296      05  WS-MVL-LEVEL-TAG         PIC  X(04) VALUE SPACES.        ELGAOL  
00297                                                                   ELGAOL  
00298  01  WS-INTERNAL-TAB-SWITCHES.                                    ELGAOL  
00299      05                          PIC  X.                          ELGAOL  
00300          88  WS-HAS-AN-INTERNAL             VALUE 'Y'.            ELGAOL  
00301          88  WS-HAS-NO-INTERNAL             VALUE 'N'.            ELGAOL  
00302                                                                   ELGAOL  
00303      05                          PIC  X.                          ELGAOL  
00304          88  WS-HAS-IBGR                    VALUE 'Y'.            ELGAOL  
00305          88  WS-HAS-NO-IBGR                 VALUE 'N'.            ELGAOL  
00306                                                                   ELGAOL  
00307      05                          PIC  X.                          ELGAOL  
00308          88  WS-HAS-IDGD                    VALUE 'Y'.            ELGAOL  
00309          88  WS-HAS-NO-IDGD                 VALUE 'N'.            ELGAOL  
00310                                                                   ELGAOL  
00311      05                          PIC  X.                          ELGAOL  
00312          88  WS-HAS-IPGN                    VALUE 'Y'.            ELGAOL  
00313          88  WS-HAS-NO-IPGN                 VALUE 'N'.            ELGAOL  
00314                                                                   ELGAOL  
00315      05                          PIC  X.                          ELGAOL  
00316          88  WS-HAS-IPGP                    VALUE 'Y'.            ELGAOL  
00317          88  WS-HAS-NO-IPGP                 VALUE 'N'.            ELGAOL  
00318                                                                   ELGAOL  
00319      05                          PIC  X.                          ELGAOL  
00320          88  WS-HAS-IPGT                    VALUE 'Y'.            ELGAOL  
00321          88  WS-HAS-NO-IPGT                 VALUE 'N'.            ELGAOL  
00322      05                          PIC  X.                          ELGAOL  
00323          88  WS-HAS-IPGS                    VALUE 'Y'.            ELGAOL  
00324          88  WS-HAS-NO-IPGS                 VALUE 'N'.            ELGAOL  
00325                                                                   ELGAOL  
00326  01  WS-INT-TAB-LST.                                              ELGAOL  
00327      02 WS-TAB-SUB                PIC S9(04) COMP.                ELGAOL  
00328      02 WS-INT-TAB                OCCURS 6 TIMES.                 ELGAOL  
00329         03                        PIC  X(18).                     ELGAOL  
00330            88 WS-INT-TAB-IBGR VALUE ' BENEFIT PROVISION'.         ELGAOL  
00331            88 WS-INT-TAB-IDGD VALUE '         DIAGNOSIS'.         ELGAOL  
00332            88 WS-INT-TAB-IPGN VALUE '   PROVIDER NUMBER'.         ELGAOL  
00333            88 WS-INT-TAB-IPGP VALUE '         PROCEDURE'.         ELGAOL  
00334            88 WS-INT-TAB-IPGT VALUE '     PROVIDER TYPE'.         ELGAOL  
00335            88 WS-INT-TAB-IPGS VALUE 'PROVIDER SPECIALTY'.         ELGAOL  
00336         03                        PIC  X(05).                     ELGAOL  
00337            88 WS-INT-TAB-AND      VALUE ' AND '.                  ELGAOL  
00338            88 WS-INT-TAB-COMMA    VALUE ',    '.                  ELGAOL  
00339            88 WS-INT-TAB-END      VALUE SPACES.                   ELGAOL  
00340                                                                   ELGAOL  
00341  01  WS-INT-TAB-TXT               REDEFINES WS-INT-TAB-LST.       ELGAOL  
00342      02                           PIC S9(04) COMP.                ELGAOL  
00343      02 WS-INT-TAB-TXT-1          PICTURE  X(69).                 ELGAOL  
00344      02 WS-INT-TAB-TXT-2          PICTURE  X(46).                 ELGAOL  
00345                                                                   ELGAOL  
00346  01  WS-TEXT-HOLD-AREA.                                           ELGAOL  
00347      02  WS-TEXT-HOLD-COUNT      PIC S9(4) COMP.                  ELGAOL  
00348      02  WS-TEXT-HOLD-TEXT       PIC X(1580).                     ELGAOL  
00349                                                                   ELGAOL  
00350  01  COUNTERS.                                                    ELGAOL  
00351      02  WS-VLT-SUB              PIC S9(4) COMP.                  ELGAOL  
00352                                                                   ELGAOL  
00353      COPY ELSVLTGC.                                               ELGAOL  
00354                                                                   ELGAOL  
00355  LINKAGE SECTION.                                                 ELGAOL  
00356  01  DFHCOMMAREA.                                                 ELGAOL  
00357      COPY ELSCOMMC.                                               ELGAOL  
00358                                                                   ELGAOL  
00359      COPY ELSCIA2C.                                               ELGAOL  
00360                                                                   ELGAOL  
00361      COPY ELSIOPMC.                                               ELGAOL  
00362                                                                   ELGAOL  
00363      COPY ELSCMIFC.                                               ELGAOL  
00364                                                                   ELGAOL  
00365      COPY ELSCMDSC.                                               ELGAOL  
00366                                                                   ELGAOL  
00367      COPY ELSOUTPC.                                               ELGAOL  
00368                                                                   ELGAOL  
00369      COPY ELSSRTPC.                                               ELGAOL  
00370                                                                   ELGAOL  
00371      COPY ELSSSCBC.                                               ELGAOL  
00372                                                                   ELGAOL  
00373      COPY ELSTCWAC.                                               ELGAOL  
00374                                                                   ELGAOL  
00375      COPY ELSACUMC.                                               ELGAOL  
00376                                                                   ELGAOL  
00377 /***********************************************************      ELGAOL  
00378 *                                                          *      ELGAOL  
00379 *                    PROCEDURE DIVISION                    *      ELGAOL  
00380 *                                                          *      ELGAOL  
00381 ************************************************************      ELGAOL  
00382  PROCEDURE DIVISION.                                              ELGAOL  
00383                                                                   ELGAOL  
00384                                                                   ELGAOL  
00385 ************************************************************      ELGAOL  
00386 *                                                          *      ELGAOL  
00387 *        PROCESS ELGAOL                                    *      ELGAOL  
00388 *                                                          *      ELGAOL  
00389 ************************************************************      ELGAOL  
00390      PERFORM 0010-INITIALIZATION THRU 0010-END.                   ELGAOL  
00391      PERFORM 0180-PROCESS THRU 0180-END.                          ELGAOL  
00392      GOBACK.                                                      ELGAOL  
00393                                                                   ELGAOL  
00394 ************************************************************      ELGAOL  
00395 *                                                          *      ELGAOL  
00396 *        INITIALIZATION                                    *      ELGAOL  
00397 *                                                          *      ELGAOL  
00398 ************************************************************      ELGAOL  
00399  0010-INITIALIZATION.                                             ELGAOL  
00400      PERFORM 0020-EST-ADR-CNTL-BLKS THRU 0020-END.                ELGAOL  
00401      PERFORM 0090-EST-ADR-WORK-AREA THRU 0090-END.                ELGAOL  
00402      IF COF-NBR-DTL-LINES > 0                                     ELGAOL  
00403      THEN                                                         ELGAOL  
00404         CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.      ELGAOL  
00405                                                                   ELGAOL  
00406      INITIALIZE TCAR-FROM-AREA                                    ELGAOL  
00407                 TCAR-FROM-LENGTH                                  ELGAOL  
00408                 TCAR-FROM-SUB.                                    ELGAOL  
00409                                                                   ELGAOL  
00410    0010-END.                                                      ELGAOL  
00411      EXIT.                                                        ELGAOL  
00412                                                                   ELGAOL  
00413                                                                   ELGAOL  
00414 /***********************************************************      ELGAOL  
00415 *                                                          *      ELGAOL  
00416 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELGAOL  
00417 *                                                          *      ELGAOL  
00418 ************************************************************      ELGAOL  
00419  0020-EST-ADR-CNTL-BLKS.                                          ELGAOL  
00420 *    *-----------------------------------------------------------*ELGAOL  
00421 *    *  PERFORMED BY 0010-INITIALIZATION.                        *ELGAOL  
00422 *    *-----------------------------------------------------------*ELGAOL  
00423 *                                                          *      ELGAOL  
00424 *        CHECK FOR VALID COMMAREA                          *      ELGAOL  
00425 *                                                          *      ELGAOL  
00426      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGAOL  
00427         EXEC CICS ABEND                                           ELGAOL  
00428                ABCODE('EL01')                                     ELGAOL  
00429         END-EXEC                                                  ELGAOL  
00430      ELSE                                                         ELGAOL  
00431          IF ECA-CIA-PTR = NULL                                    ELGAOL  
00432             EXEC CICS ABEND                                       ELGAOL  
00433                       ABCODE('EL02')                              ELGAOL  
00434             END-EXEC                                              ELGAOL  
00435          ELSE                                                     ELGAOL  
00436             CALL 'ELUINISM' USING DFHCOMMAREA                     ELGAOL  
00437                  ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA         ELGAOL  
00438             END-CALL                                              ELGAOL  
00439             SET CIA-ELSSSCB-DDN TO TRUE                           ELGAOL  
00440             CALL 'ELUSETAD' USING DFHCOMMAREA                     ELGAOL  
00441                   ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.         ELGAOL  
00442             IF CIA-RC-PTR-NULL                                    ELGAOL  
00443                PERFORM 0160-SIGNAL-UNALLOC-AREA-ERROR             ELGAOL  
00444                   THRU 0160-END.                                  ELGAOL  
00445    0020-END.                                                      ELGAOL  
00446      EXIT.                                                        ELGAOL  
00447                                                                   ELGAOL  
00448 /***********************************************************      ELGAOL  
00449 *                                                          *      ELGAOL  
00450 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELGAOL  
00451 *                                                          *      ELGAOL  
00452 ************************************************************      ELGAOL  
00453  0090-EST-ADR-WORK-AREA.                                          ELGAOL  
00454 *    *-----------------------------------------------------------*ELGAOL  
00455 *    *  PERFORMED BY 0010-INITIALIZATION.                        *ELGAOL  
00456 *    *-----------------------------------------------------------*ELGAOL  
00457      PERFORM 0100-EST-ADR-TEMP-FILE THRU 0100-END.                ELGAOL  
00458      PERFORM 0110-EST-ADR-CMIF THRU 0110-END.                     ELGAOL  
00459      PERFORM 0120-EST-ADR-OUTPUT-INTRFACE THRU 0120-END.          ELGAOL  
00460      PERFORM 0130-EST-ADR-SUBROUTINE-PARAMS THRU 0130-END.        ELGAOL  
00461      PERFORM 0140-EST-ADR-TCAR-WORK-AREA THRU 0140-END.           ELGAOL  
00462    0090-END.                                                      ELGAOL  
00463      EXIT.                                                        ELGAOL  
00464                                                                   ELGAOL  
00465                                                                   ELGAOL  
00466 ************************************************************      ELGAOL  
00467 *                                                          *      ELGAOL  
00468 *        ESTABLISH ADDRESSABILITY OF TMPORARY FILE         *      ELGAOL  
00469 *                                                          *      ELGAOL  
00470 ************************************************************      ELGAOL  
00471  0100-EST-ADR-TEMP-FILE.                                          ELGAOL  
00472 *    *-----------------------------------------------------------*ELGAOL  
00473 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREA.                     *ELGAOL  
00474 *    *-----------------------------------------------------------*ELGAOL  
00475      SET CIA-ELSWKFL1-DDN  TO  TRUE.                              ELGAOL  
00476      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOL  
00477             ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.               ELGAOL  
00478      IF CIA-RC-PTR-NULL                                           ELGAOL  
00479          PERFORM 0160-SIGNAL-UNALLOC-AREA-ERROR THRU 0160-END.    ELGAOL  
00480    0100-END.                                                      ELGAOL  
00481      EXIT.                                                        ELGAOL  
00482                                                                   ELGAOL  
00483                                                                   ELGAOL  
00484 /***********************************************************      ELGAOL  
00485 *                                                          *      ELGAOL  
00486 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE*      ELGAOL  
00487 *                                                          *      ELGAOL  
00488 ************************************************************      ELGAOL  
00489  0110-EST-ADR-CMIF.                                               ELGAOL  
00490 *    *-----------------------------------------------------------*ELGAOL  
00491 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREA.                     *ELGAOL  
00492 *    *-----------------------------------------------------------*ELGAOL  
00493      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGAOL  
00494      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOL  
00495              ADDRESS OF CMF-CODES-MANUAL-INTERFACE.               ELGAOL  
00496      IF CIA-RC-PTR-NULL                                           ELGAOL  
00497          PERFORM 0160-SIGNAL-UNALLOC-AREA-ERROR THRU 0160-END.    ELGAOL  
00498    0110-END.                                                      ELGAOL  
00499      EXIT.                                                        ELGAOL  
00500                                                                   ELGAOL  
00501                                                                   ELGAOL  
00502 ************************************************************      ELGAOL  
00503 *                                                          *      ELGAOL  
00504 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGAOL  
00505 *                                                          *      ELGAOL  
00506 ************************************************************      ELGAOL  
00507  0120-EST-ADR-OUTPUT-INTRFACE.                                    ELGAOL  
00508 *    *-----------------------------------------------------------*ELGAOL  
00509 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREA.                     *ELGAOL  
00510 *    *-----------------------------------------------------------*ELGAOL  
00511      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGAOL  
00512      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOL  
00513              ADDRESS OF COF-OUTPUT-INTERFACE.                     ELGAOL  
00514      IF CIA-RC-PTR-NULL                                           ELGAOL  
00515          PERFORM 0160-SIGNAL-UNALLOC-AREA-ERROR THRU 0160-END.    ELGAOL  
00516    0120-END.                                                      ELGAOL  
00517      EXIT.                                                        ELGAOL  
00518                                                                   ELGAOL  
00519                                                                   ELGAOL  
00520 /***********************************************************      ELGAOL  
00521 *                                                          *      ELGAOL  
00522 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGAOL  
00523 *                                                          *      ELGAOL  
00524 ************************************************************      ELGAOL  
00525  0130-EST-ADR-SUBROUTINE-PARAMS.                                  ELGAOL  
00526 *    *-----------------------------------------------------------*ELGAOL  
00527 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREA.                     *ELGAOL  
00528 *    *-----------------------------------------------------------*ELGAOL  
00529      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGAOL  
00530      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOL  
00531              ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                ELGAOL  
00532      IF CIA-RC-PTR-NULL                                           ELGAOL  
00533          PERFORM 0160-SIGNAL-UNALLOC-AREA-ERROR THRU 0160-END.    ELGAOL  
00534    0130-END.                                                      ELGAOL  
00535      EXIT.                                                        ELGAOL  
00536                                                                   ELGAOL  
00537                                                                   ELGAOL  
00538 ************************************************************      ELGAOL  
00539 *                                                          *      ELGAOL  
00540 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELGAOL  
00541 *                                                          *      ELGAOL  
00542 ************************************************************      ELGAOL  
00543  0140-EST-ADR-TCAR-WORK-AREA.                                     ELGAOL  
00544 *    *-----------------------------------------------------------*ELGAOL  
00545 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREA.                     *ELGAOL  
00546 *    *-----------------------------------------------------------*ELGAOL  
00547      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGAOL  
00548      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOL  
00549              ADDRESS OF TCAR-COMPRESSION-WORK-AREA.               ELGAOL  
00550      IF CIA-RC-PTR-NULL                                           ELGAOL  
00551          PERFORM 0160-SIGNAL-UNALLOC-AREA-ERROR THRU 0160-END.    ELGAOL  
00552    0140-END.                                                      ELGAOL  
00553      EXIT.                                                        ELGAOL  
00554                                                                   ELGAOL  
00555                                                                   ELGAOL  
00556 /***********************************************************      ELGAOL  
00557 *                                                          *      ELGAOL  
00558 *        SIGNAL UNALLOC AREA ERROR                         *      ELGAOL  
00559 *                                                          *      ELGAOL  
00560 ************************************************************      ELGAOL  
00561  0160-SIGNAL-UNALLOC-AREA-ERROR.                                  ELGAOL  
00562 *    *-----------------------------------------------------------*ELGAOL  
00563 *    *  PERFORMED BY 0080-EST-ADR,                               *ELGAOL  
00564 *    *      0100-EST-ADR-TEMP-FILE,                              *ELGAOL  
00565 *    *      0110-EST-ADR,                                        *ELGAOL  
00566 *    *      0120-EST-ADR-OUTPUT-INTRFACE,                        *ELGAOL  
00567 *    *      0130-EST-ADR-SUBROUTINE-PARAMS,                      *ELGAOL  
00568 *    *      0140-EST-ADR-TCAR-WORK-AREA.                         *ELGAOL  
00569 *    *-----------------------------------------------------------*ELGAOL  
00570      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGAOL  
00571      PERFORM 0170-SIGNAL-ABEND THRU 0170-END.                     ELGAOL  
00572    0160-END.                                                      ELGAOL  
00573      EXIT.                                                        ELGAOL  
00574                                                                   ELGAOL  
00575                                                                   ELGAOL  
00576 /***********************************************************      ELGAOL  
00577 *                                                          *      ELGAOL  
00578 *        SIGNAL ABEND                                      *      ELGAOL  
00579 *                                                          *      ELGAOL  
00580 ************************************************************      ELGAOL  
00581  0170-SIGNAL-ABEND.                                               ELGAOL  
00582 *    *-----------------------------------------------------------*ELGAOL  
00583 *    *  PERFORMED BY 0160-SIGNAL-UNALLOC-AREA-ERROR.             *ELGAOL  
00584 *    *-----------------------------------------------------------*ELGAOL  
00585      EXEC CICS ABEND                                              ELGAOL  
00586                ABCODE(CIA-ABCODE)                                 ELGAOL  
00587         END-EXEC.                                                 ELGAOL  
00588    0170-END.                                                      ELGAOL  
00589      EXIT.                                                        ELGAOL  
00590                                                                   ELGAOL  
00591                                                                   ELGAOL  
00592 /***********************************************************      ELGAOL  
00593 *                                                          *      ELGAOL  
00594 *        PROCESS                                           *      ELGAOL  
00595 *                                                          *      ELGAOL  
00596 ************************************************************      ELGAOL  
00597  0180-PROCESS.                                                    ELGAOL  
00598      IF SRP-TOPIC-ACCUM                                           ELGAOL  
00599          PERFORM 0980-OBTAIN-TOPIC-HEADER THRU 0980-END           ELGAOL  
00600      ELSE IF SRP-BEN-PROV-ACCUM                                   ELGAOL  
00601              PERFORM 0190-OBTAIN-BENEFIT-PROVISIONX THRU 0190-END.ELGAOL  
00602      IF SRP-NO-ACCUMS-FOUND                                       ELGAOL  
00603          PERFORM 0200-DISPLAY-NO-ACCUMS-MESSAGE THRU 0200-END     ELGAOL  
00604      ELSE IF SRP-INST-NOT-APPLICABLE OR                           ELGAOL  
00605              SRP-PROF-NOT-APPLICABLE                              ELGAOL  
00606                  PERFORM 0210-DISPLAY-NOT-APPLICABLE-LO THRU      ELGAOL  
00607                          0210-END                                 ELGAOL  
00608           ELSE                                                    ELGAOL  
00609                  PERFORM 0250-DISPLAY-REGULAR-TEXT THRU 0250-END. ELGAOL  
00610      IF SRP-TOPIC-ACCUM                                           ELGAOL  
00611          PERFORM 1200-END-THE-DISPLAY THRU 1200-END.              ELGAOL  
00612    0180-END.                                                      ELGAOL  
00613      EXIT.                                                        ELGAOL  
00614                                                                   ELGAOL  
00615                                                                   ELGAOL  
00616 /***********************************************************      ELGAOL  
00617 *                                                          *      ELGAOL  
00618 *        OBTAIN BENEFIT PROVISION HEADER                   *      ELGAOL  
00619 *                                                          *      ELGAOL  
00620 ************************************************************      ELGAOL  
00621  0190-OBTAIN-BENEFIT-PROVISIONX.                                  ELGAOL  
00622 *    *-----------------------------------------------------------*ELGAOL  
00623 *    *  PERFORMED BY 0180-PROCESS.                               *ELGAOL  
00624 *    *-----------------------------------------------------------*ELGAOL  
00625      INITIALIZE COF-DTL-LINE (1).                                 ELGAOL  
00626      MOVE BENEFIT-PROVISION-HEADER TO COF-DTL-LINE (2).           ELGAOL  
00627      SET  COF-CONTINUE  TO  TRUE.                                 ELGAOL  
00628      MOVE +0            TO  COF-NBR-HDR-LINES.                    ELGAOL  
00629      MOVE  2            TO  COF-NBR-DTL-LINES.                    ELGAOL  
00630      PERFORM 0990-SETUP-FOR-OUTPUT-LINK THRU 0990-END.            ELGAOL  
00631    0190-END.                                                      ELGAOL  
00632      EXIT.                                                        ELGAOL  
00633                                                                   ELGAOL  
00634                                                                   ELGAOL  
00635 ************************************************************      ELGAOL  
00636 *                                                          *      ELGAOL  
00637 *        DISPLAY NO ACCUMS MESSAGE                         *      ELGAOL  
00638 *                                                          *      ELGAOL  
00639 ************************************************************      ELGAOL  
00640  0200-DISPLAY-NO-ACCUMS-MESSAGE.                                  ELGAOL  
00641 *    *-----------------------------------------------------------*ELGAOL  
00642 *    *  PERFORMED BY 0180-PROCESS.                               *ELGAOL  
00643 *    *-----------------------------------------------------------*ELGAOL  
00644      ADD  +1             TO  TCAR-FROM-SUB.                       ELGAOL  
00645      MOVE NO-ACCUMS-MSG  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).      ELGAOL  
00646      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGAOL  
00647      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGAOL  
00648    0200-END.                                                      ELGAOL  
00649      EXIT.                                                        ELGAOL  
00650                                                                   ELGAOL  
00651                                                                   ELGAOL  
00652 /***********************************************************      ELGAOL  
00653 *                                                          *      ELGAOL  
00654 *        DISPLAY NOT APPLICABLE LOB MESSAGE                *      ELGAOL  
00655 *                                                          *      ELGAOL  
00656 ************************************************************      ELGAOL  
00657  0210-DISPLAY-NOT-APPLICABLE-LO.                                  ELGAOL  
00658 *    *-----------------------------------------------------------*ELGAOL  
00659 *    *  PERFORMED BY 0180-PROCESS.                               *ELGAOL  
00660 *    *-----------------------------------------------------------*ELGAOL  
00661      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
00662      IF SRP-INST-NOT-APPLICABLE                                   ELGAOL  
00663         MOVE NO-INST-LOB-MSG  TO  TCAR-FROM-LINE (TCAR-FROM-SUB)  ELGAOL  
00664      ELSE                                                         ELGAOL  
00665         MOVE NO-PROF-LOB-MSG  TO  TCAR-FROM-LINE (TCAR-FROM-SUB). ELGAOL  
00666      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGAOL  
00667      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGAOL  
00668    0210-END.                                                      ELGAOL  
00669      EXIT.                                                        ELGAOL  
00670                                                                   ELGAOL  
00671                                                                   ELGAOL  
00672 ************************************************************      ELGAOL  
00673 *                                                          *      ELGAOL  
00674 *        DISPLAY REGULAR TEXT                              *      ELGAOL  
00675 *                                                          *      ELGAOL  
00676 ************************************************************      ELGAOL  
00677  0250-DISPLAY-REGULAR-TEXT.                                       ELGAOL  
00678 *    *-----------------------------------------------------------*ELGAOL  
00679 *    *  PERFORMED BY 0180-PROCESS.                               *ELGAOL  
00680 *    *-----------------------------------------------------------*ELGAOL  
00681      MOVE +1      TO  IOP-TSQ-ITEM-NBR.                           ELGAOL  
00682      SET  IOP-RD  TO  TRUE.                                       ELGAOL  
00683      PERFORM 0970-SETUP-AND-READ-FILE THRU 0970-END.              ELGAOL  
00684      PERFORM 0260-DISPLAY-OCCURENCE-TEXT THRU 0260-END            ELGAOL  
00685          UNTIL NOT IOP-RC-OK.                                     ELGAOL  
00686      PERFORM 1000-DELETE-ACCUM-OCCURENCE-FI THRU 1000-END.        ELGAOL  
00687    0250-END.                                                      ELGAOL  
00688      EXIT.                                                        ELGAOL  
00689                                                                   ELGAOL  
00690                                                                   ELGAOL  
00691 /***********************************************************      ELGAOL  
00692 *                                                          *      ELGAOL  
00693 *        DISPLAY OCCURENCE TEXT                            *      ELGAOL  
00694 *                                                          *      ELGAOL  
00695 ************************************************************      ELGAOL  
00696  0260-DISPLAY-OCCURENCE-TEXT.                                     ELGAOL  
00697 *    *-----------------------------------------------------------*ELGAOL  
00698 *    *  PERFORMED BY 0250-DISPLAY-REGULAR-TEXT.                  *ELGAOL  
00699 *    *-----------------------------------------------------------*ELGAOL  
00700      PERFORM 0270-PROCESS-PERCENT-PHRASE THRU 0270-END.           ELGAOL  
00701      PERFORM 0500-CREATE-O-P-X-APPLIC THRU 0500-END.              ELGAOL  
00702      IF NOT DEFINITION-NA                                         ELGAOL  
00703          PERFORM 0800-CREATE-DEFINITION-SENTENC THRU 0800-END.    ELGAOL  
00704      IF NOT BENEFIT-PERIOD-NA                                     ELGAOL  
00705          PERFORM 0830-CREATE-BENEFIT-PERIOD-SEN THRU 0830-END.    ELGAOL  
00706      IF NOT CARRY-OVER-CREDIT-IND-NA                              ELGAOL  
00707          PERFORM 0840-CREATE-CARRY-OVR THRU 0840-END.             ELGAOL  
00708      IF WS-HAS-AN-INTERNAL                                        ELGAOL  
00709          PERFORM 0850-CALL-INTERNALS THRU 0850-END.               ELGAOL  
00710      IF SRP-TOPIC-ACCUM                                           ELGAOL  
00711          PERFORM 0960-DISPLAY-O-P-X-END-SENTNCE THRU 0960-END.    ELGAOL  
00712      SET IOP-RD-NXT TO  TRUE.                                     ELGAOL  
00713      PERFORM 0970-SETUP-AND-READ-FILE THRU 0970-END.              ELGAOL  
00714      IF SRP-TOPIC-ACCUM AND IOP-RC-OK                             ELGAOL  
00715          PERFORM 0980-OBTAIN-TOPIC-HEADER THRU 0980-END.          ELGAOL  
00716    0260-END.                                                      ELGAOL  
00717      EXIT.                                                        ELGAOL  
00718                                                                   ELGAOL  
00719                                                                   ELGAOL  
00720 /***********************************************************      ELGAOL  
00721 *                                                          *      ELGAOL  
00722 *        PROCESS PERCENT PHRASE                            *      ELGAOL  
00723 *                                                          *      ELGAOL  
00724 ************************************************************      ELGAOL  
00725  0270-PROCESS-PERCENT-PHRASE.                                     ELGAOL  
00726 *    *-----------------------------------------------------------*ELGAOL  
00727 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGAOL  
00728 *    *-----------------------------------------------------------*ELGAOL  
00729      PERFORM 0730-INITIALIZE-SWITCHES      THRU 0730-END.         ELGAOL  
00730      PERFORM 0740-INITIALIZE-COMPRESS-AREA THRU 0740-END.         ELGAOL  
00731      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGAOL  
00732          PERFORM 0300-CREATE-SINGLE-PCENT-PHRA                    ELGAOL  
00733                  THRU 0300-END                                    ELGAOL  
00734      ELSE                                                         ELGAOL  
00735          PERFORM 0310-CREATE-MULTI-PCENT-PHRA                     ELGAOL  
00736                  THRU 0310-END.                                   ELGAOL  
00737      PERFORM 0430-GET-FAM-OR-IND-PHRASE THRU 0430-END.            ELGAOL  
00738      PERFORM 0450-GET-L-O-B-PHRA THRU 0450-END.                   ELGAOL  
00739    0270-END.                                                      ELGAOL  
00740      EXIT.                                                        ELGAOL  
00741                                                                   ELGAOL  
00742                                                                   ELGAOL  
00743 ************************************************************      ELGAOL  
00744 *                                                          *      ELGAOL  
00745 *        CREATE SINGLE LEVEL PERCENT PHRASE                *      ELGAOL  
00746 *                                                          *      ELGAOL  
00747 ************************************************************      ELGAOL  
00748  0300-CREATE-SINGLE-PCENT-PHRA.                                   ELGAOL  
00749 *    *-----------------------------------------------------------*ELGAOL  
00750 *    *  PERFORMED BY 0270-PROCESS-PERCENT-PHRASE.                *ELGAOL  
00751 *    *-----------------------------------------------------------*ELGAOL  
00752      INITIALIZE WS-VAL-LIMIT-AMOUNT                               ELGAOL  
00753                 WS-SINGLE-VAL-LMT-PHR.                            ELGAOL  
00754      MOVE ACCUM-PERCENT-LEVEL (1) TO WS-SINGLE-PERCENT-LEVEL.     ELGAOL  
00755      SET ASC-DES-INDEX TO 1.                                      ELGAOL  
00756      PERFORM 0340-PROCESS-VALUE-LIMIT THRU 0340-END.              ELGAOL  
00757    0300-END.                                                      ELGAOL  
00758      EXIT.                                                        ELGAOL  
00759                                                                   ELGAOL  
00760                                                                   ELGAOL  
00761 /***********************************************************      ELGAOL  
00762 *                                                          *      ELGAOL  
00763 *        CREATE MULTI LEVEL PERCENT PHRASE                 *      ELGAOL  
00764 *                                                          *      ELGAOL  
00765 ************************************************************      ELGAOL  
00766  0310-CREATE-MULTI-PCENT-PHRA.                                    ELGAOL  
00767 *    *-----------------------------------------------------------*ELGAOL  
00768 *    *  PERFORMED BY 0270-PROCESS-PERCENT-PHRASE.                *ELGAOL  
00769 *    *-----------------------------------------------------------*ELGAOL  
00770      IF COF-NBR-DTL-LINES > 0                                     ELGAOL  
00771         PERFORM 1300-LINK-TO-OUTPUT-MODULE THRU 1300-END.         ELGAOL  
00772      ADD +1 TO TCAR-FROM-SUB.                                     ELGAOL  
00773      MOVE PC-MULTI-PCENT-START TO                                 ELGAOL  
00774           TCAR-FROM-LINE(TCAR-FROM-SUB).                          ELGAOL  
00775      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGAOL  
00776      SET ASC-DES-INDEX TO 1.                                      ELGAOL  
00777      MOVE SPACES TO WS-MVL-CHANGES-PHRASE.                        ELGAOL  
00778      PERFORM 0320-CREATE-PCENT-VALUE-PHRA THRU 0320-END.          ELGAOL  
00779      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGAOL  
00780      MOVE WS-MULTI-VALUE-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).ELGAOL  
00781      IF COF-NBR-DTL-LINES >= 20                                   ELGAOL  
00782         PERFORM 1300-LINK-TO-OUTPUT-MODULE THRU 1300-END          ELGAOL  
00783         INITIALIZE COF-DTL                                        ELGAOL  
00784         MOVE ZERO TO COF-NBR-DTL-LINES.                           ELGAOL  
00785      PERFORM 0330-ASCEND-DESCEND-LOOP THRU 0330-END               ELGAOL  
00786              UNTIL ASC-DES-INDEX = ACCUM-ASCEND-DESCEND-COUNT.    ELGAOL  
00787      IF COF-NBR-DTL-LINES > 0                                     ELGAOL  
00788         PERFORM 1300-LINK-TO-OUTPUT-MODULE THRU 1300-END.         ELGAOL  
00789      PERFORM 0740-INITIALIZE-COMPRESS-AREA THRU 0740-END.         ELGAOL  
00790    0310-END.                                                      ELGAOL  
00791      EXIT.                                                        ELGAOL  
00792                                                                   ELGAOL  
00793                                                                   ELGAOL  
00794 /***********************************************************      ELGAOL  
00795 *                                                          *      ELGAOL  
00796 *        CREATE PERCENT VALUE PHRASE                       *      ELGAOL  
00797 *                                                          *      ELGAOL  
00798 ************************************************************      ELGAOL  
00799  0320-CREATE-PCENT-VALUE-PHRA.                                    ELGAOL  
00800 *    *-----------------------------------------------------------*ELGAOL  
00801 *    *  PERFORMED BY 0310-CREATE-MULTI-PCENT-PHRA                *ELGAOL  
00802 *    *               0330-ASCEND-DESCEND-LOOP.                   *ELGAOL  
00803 *    *-----------------------------------------------------------*ELGAOL  
00804      MOVE 1 TO TCAR-FROM-SUB.                                     ELGAOL  
00805      MOVE ACCUM-PERCENT-LEVEL (ASC-DES-INDEX) TO                  ELGAOL  
00806           WS-MULTI-PERCENT-LEVEL.                                 ELGAOL  
00807      PERFORM 0340-PROCESS-VALUE-LIMIT THRU 0340-END.              ELGAOL  
00808      MOVE PC-DOTS TO WS-MVL-MASK.                                 ELGAOL  
00809      PERFORM 0700-SEND-PART-PARA THRU 0700-END.                   ELGAOL  
00810      STRING TCAR-FROM-LINE (1)                                    ELGAOL  
00811         DELIMITED BY '  ' INTO WS-MVL-MASK.                       ELGAOL  
00812      SET VLT-INDEX TO ASC-DES-INDEX.                              ELGAOL  
00813      MOVE VLT-VARIABLE-LEVEL-TAG (VLT-INDEX) TO                   ELGAOL  
00814           WS-MVL-LEVEL-TAG.                                       ELGAOL  
00815    0320-END.                                                      ELGAOL  
00816      EXIT.                                                        ELGAOL  
00817                                                                   ELGAOL  
00818                                                                   ELGAOL  
00819 /***********************************************************      ELGAOL  
00820 *                                                          *      ELGAOL  
00821 *        ASCEND-DESCEND-LOOP                               *      ELGAOL  
00822 *                                                          *      ELGAOL  
00823 ************************************************************      ELGAOL  
00824  0330-ASCEND-DESCEND-LOOP.                                        ELGAOL  
00825 *    *-----------------------------------------------------------*ELGAOL  
00826 *    *  PERFORMED BY 0310-CREATE-MULTI-PCENT-PHRA.               *ELGAOL  
00827 *    *-----------------------------------------------------------*ELGAOL  
00828      MOVE PC-CHANGES-TO TO WS-MVL-CHANGES-PHRASE.                 ELGAOL  
00829      SET ASC-DES-INDEX UP BY 1.                                   ELGAOL  
00830      PERFORM 0320-CREATE-PCENT-VALUE-PHRA THRU 0320-END.          ELGAOL  
00831      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGAOL  
00832      MOVE WS-MULTI-VALUE-LINE                                     ELGAOL  
00833        TO COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELGAOL  
00834      IF COF-NBR-DTL-LINES >= 20                                   ELGAOL  
00835         PERFORM 1300-LINK-TO-OUTPUT-MODULE THRU 1300-END          ELGAOL  
00836         INITIALIZE COF-DTL                                        ELGAOL  
00837         MOVE ZERO TO COF-NBR-DTL-LINES.                           ELGAOL  
00838    0330-END.                                                      ELGAOL  
00839      EXIT.                                                        ELGAOL  
00840                                                                   ELGAOL  
00841                                                                   ELGAOL  
00842 /***********************************************************      ELGAOL  
00843 *                                                          *      ELGAOL  
00844 *        PROCESS VALUE LIMIT                               *      ELGAOL  
00845 *                                                          *      ELGAOL  
00846 ************************************************************      ELGAOL  
00847  0340-PROCESS-VALUE-LIMIT.                                        ELGAOL  
00848 *    *-----------------------------------------------------------*ELGAOL  
00849 *    *  PERFORMED BY 0300-CREATE-SINGLE-PCENT-PHRA.              *ELGAOL  
00850 *    *  PERFORMED BY 0320-CREATE-PCENT-VALUE-PHRA.               *ELGAOL  
00851 *    *-----------------------------------------------------------*ELGAOL  
00852      IF ACCUM-VALUE-LIMIT (ASC-DES-INDEX) < 0                     ELGAOL  
00853         PERFORM 0350-DISPLAY-OUTSIDE-SOURCE THRU 0350-END         ELGAOL  
00854      ELSE                                                         ELGAOL  
00855         PERFORM 0380-GET-VALUE-LIMIT-QUALIFIER                    ELGAOL  
00856            THRU 0380-END.                                         ELGAOL  
00857      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGAOL  
00858         MOVE WS-VAL-LIMIT-AMOUNT TO WS-SINGLE-VAL-LMT-PHR         ELGAOL  
00859         ADD 1 TO TCAR-FROM-SUB                                    ELGAOL  
00860         MOVE WS-SINGLE-PERCENT-PHRASE                             ELGAOL  
00861               TO TCAR-FROM-LINE (TCAR-FROM-SUB)                   ELGAOL  
00862      ELSE                                                         ELGAOL  
00863         MOVE WS-VAL-LIMIT-AMOUNT TO WS-MULTI-VAL-LMT-PHR          ELGAOL  
00864         MOVE WS-MULTI-VALUE-PHRASE                                ELGAOL  
00865               TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGAOL  
00866      PERFORM 0750-CHECK-FOR-INTERNALS THRU 0750-END.              ELGAOL  
00867    0340-END.                                                      ELGAOL  
00868      EXIT.                                                        ELGAOL  
00869                                                                   ELGAOL  
00870                                                                   ELGAOL  
00871 /***********************************************************      ELGAOL  
00872 *                                                          *      ELGAOL  
00873 *        DISPLAY OUTSIDE SOURCE                            *      ELGAOL  
00874 *                                                          *      ELGAOL  
00875 ************************************************************      ELGAOL  
00876  0350-DISPLAY-OUTSIDE-SOURCE.                                     ELGAOL  
00877 *    *-----------------------------------------------------------*ELGAOL  
00878 *    *  PERFORMED BY 0340-PROCESS-VALUE-LIMIT                    *ELGAOL  
00879 *    *-----------------------------------------------------------*ELGAOL  
00880                                                                   ELGAOL  
00881      IF ACCUM-OPX-BASE-AMT-SOURCE-IND NOT = ZERO                  ELGAOL  
00882      THEN                                                         ELGAOL  
00883         MOVE ACCUM-OPX-BASE-AMT-SOURCE-IND TO CMF-CODE-VALUE      ELGAOL  
00884         MOVE 'OUTPKT-BASE-AMT-SOURCE-IN'                          ELGAOL  
00885           TO CMF-ELEMENT-SYSTEM-NAME                              ELGAOL  
00886         MOVE 'GROUP' TO CMF-RECORD-PREFIX                         ELGAOL  
00887         EXEC CICS LINK PROGRAM('ELUCMIF')                         ELGAOL  
00888                        COMMAREA(DFHCOMMAREA)                      ELGAOL  
00889               END-EXEC                                            ELGAOL  
00890         SET CIA-ELSCMDSC-DDN TO TRUE                              ELGAOL  
00891         CALL 'ELUSETAD' USING DFHCOMMAREA ADDRESS OF CMF-DESCR    ELGAOL  
00892         STRING PC-OUTPKT-OTHR-SRCE-LEAD DELIMITED BY SIZE,        ELGAOL  
00893           PC-THE DELIMITED BY SIZE,                               ELGAOL  
00894           CMF-DESCR-LINE (1) DELIMITED BY '  '                    ELGAOL  
00895           INTO WS-VAL-QUAL-UNLIMITED                              ELGAOL  
00896      ELSE                                                         ELGAOL  
00897         STRING PC-OUTPKT-OTHR-SRCE-LEAD DELIMITED BY SIZE,        ELGAOL  
00898           PC-ANOTHER-SOURCE DELIMITED BY SIZE                     ELGAOL  
00899           INTO WS-VAL-QUAL-UNLIMITED                              ELGAOL  
00900      END-IF.                                                      ELGAOL  
00901                                                                   ELGAOL  
00902    0350-END.                                                      ELGAOL  
00903      EXIT.                                                        ELGAOL  
00904                                                                   ELGAOL  
00905                                                                   ELGAOL  
00906 /***********************************************************      ELGAOL  
00907 *                                                          *      ELGAOL  
00908 *        GET VALUE LIMIT QUALIFIER PHRASE                  *      ELGAOL  
00909 *                                                          *      ELGAOL  
00910 ************************************************************      ELGAOL  
00911  0380-GET-VALUE-LIMIT-QUALIFIER.                                  ELGAOL  
00912 *    *-----------------------------------------------------------*ELGAOL  
00913 *    *  PERFORMED BY 0340-PROCESS-VALUE-LIMIT.                   *ELGAOL  
00914 *    *-----------------------------------------------------------*ELGAOL  
00915      MOVE ACCUM-VALUE-QUALIFIER   TO  CMF-CODE-VALUE.             ELGAOL  
00916      MOVE 'O-P-X-VALUE-QUALIFIER' TO  CMF-ELEMENT-SYSTEM-NAME.    ELGAOL  
00917      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
00918                                                                   ELGAOL  
00919      IF CMF-DESCR-LINE (1)  =  PC-DOLLARS                         ELGAOL  
00920         PERFORM 0410-BUILD-DOLLAR-VAL-LIM-PHRA THRU 0410-END      ELGAOL  
00921      ELSE PERFORM 0420-BUILD-OTHER-VAL-LIM-PHRA                   ELGAOL  
00922                   THRU 0420-END.                                  ELGAOL  
00923    0380-END.                                                      ELGAOL  
00924      EXIT.                                                        ELGAOL  
00925                                                                   ELGAOL  
00926                                                                   ELGAOL  
00927 /***********************************************************      ELGAOL  
00928 *                                                          *      ELGAOL  
00929 *        BUILD DOLLAR VALUE LIMIT PHRASE                   *      ELGAOL  
00930 *                                                          *      ELGAOL  
00931 ************************************************************      ELGAOL  
00932 *    *-----------------------------------------------------------*ELGAOL  
00933 *    *  PERFORMED BY 0380-GET-VALUE-LIMIT-QUALIFIER              *ELGAOL  
00934 *    *-----------------------------------------------------------*ELGAOL  
00935  0410-BUILD-DOLLAR-VAL-LIM-PHRA.                                  ELGAOL  
00936      MOVE SPACES TO WS-VAL-QUAL-UNLIMITED.                        ELGAOL  
00937      MOVE ACCUM-VALUE-LIMIT (ASC-DES-INDEX)                       ELGAOL  
00938           TO WS-VAL-LIM-DOLLARS.                                  ELGAOL  
00939      IF WS-MAX-LIMIT-DOLLARS                                      ELGAOL  
00940         STRING 'UNLIMITED ' DELIMITED BY SIZE, CMF-DESCR-LINE (1) ELGAOL  
00941           DELIMITED BY '  ',                                      ELGAOL  
00942           ' ' DELIMITED BY SIZE                                   ELGAOL  
00943           INTO WS-VAL-QUAL-UNLIMITED                              ELGAOL  
00944      ELSE CONTINUE.                                               ELGAOL  
00945   0410-END. EXIT.                                                 ELGAOL  
00946                                                                   ELGAOL  
00947                                                                   ELGAOL  
00948 ************************************************************      ELGAOL  
00949 *                                                          *      ELGAOL  
00950 *        BUILD OTHER VALUE LIMIT PHRASE                    *      ELGAOL  
00951 *                                                          *      ELGAOL  
00952 ************************************************************      ELGAOL  
00953 *    *-----------------------------------------------------------*ELGAOL  
00954 *    *  PERFORMED BY 0380-GET-VALUE-LIMIT-QUALIFIER              *ELGAOL  
00955 *    *-----------------------------------------------------------*ELGAOL  
00956  0420-BUILD-OTHER-VAL-LIM-PHRA.                                   ELGAOL  
00957      MOVE ACCUM-VALUE-LIMIT-NON-DOLLAR (ASC-DES-INDEX)            ELGAOL  
00958         TO WS-VAL-LIM-OTHER.                                      ELGAOL  
00959      MOVE SPACES TO WS-VAL-QUAL-OTHER.                            ELGAOL  
00960      IF WS-MAX-LIMIT-OTHER                                        ELGAOL  
00961         STRING ' UNLIMITED ' DELIMITED BY SIZE, CMF-DESCR-LINE (1)ELGAOL  
00962           DELIMITED BY '  ',                                      ELGAOL  
00963           ' ' DELIMITED BY SIZE                                   ELGAOL  
00964           INTO WS-VAL-QUAL-UNLIMITED                              ELGAOL  
00965      ELSE                                                         ELGAOL  
00966           STRING ' ' DELIMITED BY SIZE                            ELGAOL  
00967                  CMF-DESCR-LINE (1) DELIMITED BY '  '             ELGAOL  
00968                INTO WS-VAL-QUAL-OTHER.                            ELGAOL  
00969   0420-END. EXIT.                                                 ELGAOL  
00970                                                                   ELGAOL  
00971                                                                   ELGAOL  
00972                                                                   ELGAOL  
00973 /***********************************************************      ELGAOL  
00974 *                                                          *      ELGAOL  
00975 *        GET FAMILY OR INDIVIDUAL PHRASE                   *      ELGAOL  
00976 *                                                          *      ELGAOL  
00977 ************************************************************      ELGAOL  
00978  0430-GET-FAM-OR-IND-PHRASE.                                      ELGAOL  
00979 *    *-----------------------------------------------------------*ELGAOL  
00980 *    *  PERFORMED BY 0270-PROCESS-PERCENT-PHRASE                 *ELGAOL  
00981 *    *-----------------------------------------------------------*ELGAOL  
00982      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
00983      MOVE PC-PER TO TCAR-FROM-LINE (TCAR-FROM-SUB).               ELGAOL  
00984      MOVE ACCUM-FAM-OR-INDIV   TO  CMF-CODE-VALUE.                ELGAOL  
00985      MOVE 'O-P-X-FAM-OR-INDIV' TO  CMF-ELEMENT-SYSTEM-NAME.       ELGAOL  
00986      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
00987      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
00988    0430-END.                                                      ELGAOL  
00989      EXIT.                                                        ELGAOL  
00990                                                                   ELGAOL  
00991                                                                   ELGAOL  
00992 /***********************************************************      ELGAOL  
00993 *                                                          *      ELGAOL  
00994 *        GET LINE OF BUSINESS PHRASE                       *      ELGAOL  
00995 *                                                          *      ELGAOL  
00996 ************************************************************      ELGAOL  
00997  0450-GET-L-O-B-PHRA.                                             ELGAOL  
00998 *    *-----------------------------------------------------------*ELGAOL  
00999 *    *  PERFORMED BY 0270-PROCESS-PERCENT-PHRASE                 *ELGAOL  
01000 *    *-----------------------------------------------------------*ELGAOL  
01001      ADD  +1      TO  TCAR-FROM-SUB.                              ELGAOL  
01002      MOVE PC-FOR  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGAOL  
01003      MOVE ACCUM-L-O-B   TO  CMF-CODE-VALUE.                       ELGAOL  
01004      MOVE 'O-P-X-L-O-B' TO  CMF-ELEMENT-SYSTEM-NAME.              ELGAOL  
01005      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01006      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
01007      ADD  +1                    TO  TCAR-FROM-SUB.                ELGAOL  
01008      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGAOL  
01009      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGAOL  
01010      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGAOL  
01011    0450-END.                                                      ELGAOL  
01012      EXIT.                                                        ELGAOL  
01013                                                                   ELGAOL  
01014                                                                   ELGAOL  
01015 /***********************************************************      ELGAOL  
01016 *                                                          *      ELGAOL  
01017 *        CREATE OUT-OF-POCKET APPLICABILITY SENTENCE       *      ELGAOL  
01018 *                                                          *      ELGAOL  
01019 ************************************************************      ELGAOL  
01020  0500-CREATE-O-P-X-APPLIC.                                        ELGAOL  
01021 *    *-----------------------------------------------------------*ELGAOL  
01022 *    *  PERFORMED BY 0260-DISPLAY-OCCURRENCE-TEXT                *ELGAOL  
01023 *    *-----------------------------------------------------------*ELGAOL  
01024      ADD   +1      TO  TCAR-FROM-SUB.                             ELGAOL  
01025      MOVE PC-THIS  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGAOL  
01026      IF  FYI-VALUE-NA                                             ELGAOL  
01027          CONTINUE                                                 ELGAOL  
01028      ELSE PERFORM 0520-GET-FYI-PHRASE THRU 0520-END.              ELGAOL  
01029      ADD +1 TO  TCAR-FROM-SUB.                                    ELGAOL  
01030      MOVE   PC-OUT-OF-POCKET-APPLIES                              ELGAOL  
01031             TO TCAR-FROM-LINE (TCAR-FROM-SUB).                    ELGAOL  
01032      ADD +1 TO  TCAR-FROM-SUB.                                    ELGAOL  
01033      IF COST-CONTAIN-IND-NA                                       ELGAOL  
01034          ADD +1 TO  TCAR-FROM-SUB                                 ELGAOL  
01035          MOVE PC-SERVICES TO TCAR-FROM-LINE (TCAR-FROM-SUB)       ELGAOL  
01036      ELSE                                                         ELGAOL  
01037          PERFORM 0560-GET-COST-CONTAINMENT-PHRA THRU 0560-END.    ELGAOL  
01038      ADD +1 TO  TCAR-FROM-SUB.                                    ELGAOL  
01039      MOVE PC-FOR  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGAOL  
01040      PERFORM 0600-GET-CONDITION-BITS-PHRASE THRU 0600-END.        ELGAOL  
01041      ADD +1 TO  TCAR-FROM-SUB.                                    ELGAOL  
01042      MOVE PC-PROVIDED  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).        ELGAOL  
01043      IF PLACE-OF-TREATMENT-NA                                     ELGAOL  
01044         CONTINUE                                                  ELGAOL  
01045      ELSE                                                         ELGAOL  
01046          PERFORM 0620-GET-PLACE-OF-TREATMENT-PH THRU 0620-END.    ELGAOL  
01047      EVALUATE    AGE-LMT-FROM-IND-NA                              ELGAOL  
01048             ALSO AGE-LMT-TO-IND-NA                                ELGAOL  
01049             ALSO RELATIONSHIP-IND-NA                              ELGAOL  
01050         WHEN TRUE ALSO TRUE ALSO TRUE                             ELGAOL  
01051            CONTINUE                                               ELGAOL  
01052         WHEN TRUE ALSO TRUE ALSO FALSE                            ELGAOL  
01053           PERFORM 0640-GET-RELATSHIP-IND-PHR THRU 0640-END        ELGAOL  
01054         WHEN OTHER                                                ELGAOL  
01055           PERFORM 0660-GET-AGE-AND-REL-PHR THRU 0660-END          ELGAOL  
01056      END-EVALUATE.                                                ELGAOL  
01057                                                                   ELGAOL  
01058      IF WS-HAS-AN-INTERNAL                                        ELGAOL  
01059         PERFORM 0760-GET-INTERNAL-TABULARS-LIS THRU 0760-END.     ELGAOL  
01060      ADD  +1                    TO  TCAR-FROM-SUB.                ELGAOL  
01061      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGAOL  
01062      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGAOL  
01063      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGAOL  
01064    0500-END.                                                      ELGAOL  
01065      EXIT.                                                        ELGAOL  
01066                                                                   ELGAOL  
01067                                                                   ELGAOL  
01068 /***********************************************************      ELGAOL  
01069 *                                                          *      ELGAOL  
01070 *        GET FYI PHRASE                                    *      ELGAOL  
01071 *                                                          *      ELGAOL  
01072 ************************************************************      ELGAOL  
01073  0520-GET-FYI-PHRASE.                                             ELGAOL  
01074 *    *-----------------------------------------------------------*ELGAOL  
01075 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGAOL  
01076 *    *-----------------------------------------------------------*ELGAOL  
01077      MOVE ACCUM-FYI-VALUE   TO  CMF-CODE-VALUE.                   ELGAOL  
01078      MOVE 'O-P-X-FYI-VALUE' TO                                    ELGAOL  
01079          CMF-ELEMENT-SYSTEM-NAME.                                 ELGAOL  
01080      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01081      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
01082    0520-END.                                                      ELGAOL  
01083      EXIT.                                                        ELGAOL  
01084                                                                   ELGAOL  
01085                                                                   ELGAOL  
01086 /***********************************************************      ELGAOL  
01087 *                                                          *      ELGAOL  
01088 *        GET COST CONTAINMENT PHRASE                       *      ELGAOL  
01089 *                                                          *      ELGAOL  
01090 ************************************************************      ELGAOL  
01091  0560-GET-COST-CONTAINMENT-PHRA.                                  ELGAOL  
01092 *    *-----------------------------------------------------------*ELGAOL  
01093 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGAOL  
01094 *    *-----------------------------------------------------------*ELGAOL  
01095      MOVE ACCUM-COST-CONTAIN-IND   TO  CMF-CODE-VALUE.            ELGAOL  
01096      MOVE 'O-P-X-COST-CONTAIN-IND' TO  CMF-ELEMENT-SYSTEM-NAME.   ELGAOL  
01097      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01098      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
01099    0560-END.                                                      ELGAOL  
01100      EXIT.                                                        ELGAOL  
01101                                                                   ELGAOL  
01102                                                                   ELGAOL  
01103 /***********************************************************      ELGAOL  
01104 *                                                          *      ELGAOL  
01105 *        GET CONDITION BITS PHRASE                         *      ELGAOL  
01106 *                                                          *      ELGAOL  
01107 ************************************************************      ELGAOL  
01108  0600-GET-CONDITION-BITS-PHRASE.                                  ELGAOL  
01109 *    *-----------------------------------------------------------*ELGAOL  
01110 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGAOL  
01111 *    *-----------------------------------------------------------*ELGAOL  
01112 * -- PRESERVE CONTENTS OF TCAR-FROM-AREA                          ELGAOL  
01113 *    (ELUCONDB USES THE TEXT COMPRESSION WORK AREA, WHICH CAUSES  ELGAOL  
01114 *    THE LOSS OF ANYTHING IN TCAR-FROM-AREA.  WE MUST SAVE THE    ELGAOL  
01115 *    CURRENT CONTENT OF TCAR-FROM-AREA AND RESTORE IT AFTER       ELGAOL  
01116 *    OBTAINING THE TRANSLATION OF THE CONDITION BITS.)            ELGAOL  
01117      MOVE TCAR-FROM-SUB TO WS-TEXT-HOLD-COUNT.                    ELGAOL  
01118      MOVE TCAR-FROM-AREA TO WS-TEXT-HOLD-TEXT.                    ELGAOL  
01119                                                                   ELGAOL  
01120 * -- GET CONDITION BIT TRANSLATION                                ELGAOL  
01121      MOVE ACCUM-CONDITION  TO  CMF-CONDITION-BITS.                ELGAOL  
01122      CALL 'ELUCONDB' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGAOL  
01123                                                                   ELGAOL  
01124      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGAOL  
01125      CALL 'ELUSETAD'                                              ELGAOL  
01126         USING DFHCOMMAREA                                         ELGAOL  
01127               ADDRESS OF CMF-DESCR.                               ELGAOL  
01128                                                                   ELGAOL  
01129 * -- RESTORE THE CONTENTS OF TCAR-FROM-AREA                       ELGAOL  
01130      MOVE WS-TEXT-HOLD-COUNT TO TCAR-FROM-SUB.                    ELGAOL  
01131      MOVE WS-TEXT-HOLD-TEXT TO TCAR-FROM-AREA.                    ELGAOL  
01132                                                                   ELGAOL  
01133 * -- APPEND THE TRANSLATION                                       ELGAOL  
01134      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
01135    0600-END.                                                      ELGAOL  
01136      EXIT.                                                        ELGAOL  
01137                                                                   ELGAOL  
01138                                                                   ELGAOL  
01139 /***********************************************************      ELGAOL  
01140 *                                                          *      ELGAOL  
01141 *        GET PLACE OF TREATMENT PHRASE                     *      ELGAOL  
01142 *                                                          *      ELGAOL  
01143 ************************************************************      ELGAOL  
01144  0620-GET-PLACE-OF-TREATMENT-PH.                                  ELGAOL  
01145 *    *-----------------------------------------------------------*ELGAOL  
01146 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGAOL  
01147 *    *-----------------------------------------------------------*ELGAOL  
01148      MOVE ACCUM-PLACE-OF-TREATMENT  TO  CMF-CODE-VALUE.           ELGAOL  
01149      MOVE 'O-P-X-PLACE-OF-TREATMENT' TO CMF-ELEMENT-SYSTEM-NAME.  ELGAOL  
01150      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01151      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
01152    0620-END.                                                      ELGAOL  
01153      EXIT.                                                        ELGAOL  
01154                                                                   ELGAOL  
01155                                                                   ELGAOL  
01156 /***********************************************************      ELGAOL  
01157 *                                                          *      ELGAOL  
01158 *        GET RELATIONSHIP INDICATOR PHRASE                 *      ELGAOL  
01159 *                                                          *      ELGAOL  
01160 ************************************************************      ELGAOL  
01161  0640-GET-RELATSHIP-IND-PHR.                                      ELGAOL  
01162 *    *-----------------------------------------------------------*ELGAOL  
01163 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGAOL  
01164 *    *-----------------------------------------------------------*ELGAOL  
01165      ADD  +1           TO  TCAR-FROM-SUB.                         ELGAOL  
01166      MOVE PC-TO        TO  TCAR-FROM-LINE (TCAR-FROM-SUB).        ELGAOL  
01167      MOVE ACCUM-RELATIONSHIP-IND    TO  CMF-CODE-VALUE.           ELGAOL  
01168      MOVE 'O-P-X-RELATIONSHIP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.  ELGAOL  
01169      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01170      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
01171    0640-END.                                                      ELGAOL  
01172      EXIT.                                                        ELGAOL  
01173                                                                   ELGAOL  
01174                                                                   ELGAOL  
01175 /***********************************************************      ELGAOL  
01176 *                                                          *      ELGAOL  
01177 *        GET AGE AND REL PHR                               *      ELGAOL  
01178 *                                                          *      ELGAOL  
01179 ************************************************************      ELGAOL  
01180  0660-GET-AGE-AND-REL-PHR.                                        ELGAOL  
01181 *    *-----------------------------------------------------------*ELGAOL  
01182 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGAOL  
01183 *    *-----------------------------------------------------------*ELGAOL  
01184      IF   RELATIONSHIP-IND-NA                                     ELGAOL  
01185           ADD +1 TO TCAR-FROM-SUB                                 ELGAOL  
01186           MOVE 'PATIENTS' TO TCAR-FROM-LINE (TCAR-FROM-SUB)       ELGAOL  
01187      ELSE                                                         ELGAOL  
01188           PERFORM 0640-GET-RELATSHIP-IND-PHR THRU 0640-END.       ELGAOL  
01189      ADD +1 TO TCAR-FROM-SUB.                                     ELGAOL  
01190      MOVE 'FROM AGE' TO TCAR-FROM-LINE (TCAR-FROM-SUB).           ELGAOL  
01191      PERFORM 0670-GET-FROM-AGE-PHRASE THRU 0670-END.              ELGAOL  
01192      ADD +1 TO TCAR-FROM-SUB.                                     ELGAOL  
01193      MOVE 'TO AGE' TO  TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGAOL  
01194      PERFORM 0680-GET-TO-AGE-PHRASE THRU 0680-END.                ELGAOL  
01195    0660-END.                                                      ELGAOL  
01196      EXIT.                                                        ELGAOL  
01197                                                                   ELGAOL  
01198                                                                   ELGAOL  
01199 /***********************************************************      ELGAOL  
01200 *                                                          *      ELGAOL  
01201 *        GET FROM AGE PHRASE                               *      ELGAOL  
01202 *                                                          *      ELGAOL  
01203 ************************************************************      ELGAOL  
01204  0670-GET-FROM-AGE-PHRASE.                                        ELGAOL  
01205 *    *-----------------------------------------------------------*ELGAOL  
01206 *    *  PERFORMED BY 0660-GET-AGE-AND-REL-PHR.                   *ELGAOL  
01207 *    *-----------------------------------------------------------*ELGAOL  
01208      IF   ACCUM-AGE-LIMIT-FROM-VAL = +999                         ELGAOL  
01209           MOVE  PC-UNLIMITED TO  WS-FROM-AGE-ALPHA                ELGAOL  
01210      ELSE                                                         ELGAOL  
01211           MOVE ACCUM-AGE-LIMIT-FROM-VAL TO WS-FROM-AGE.           ELGAOL  
01212                                                                   ELGAOL  
01213      MOVE ACCUM-AGE-LIMIT-FROM-IND  TO  CMF-CODE-VALUE.           ELGAOL  
01214      MOVE 'O-P-X-AGE-QUAL-IND-FROM' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGAOL  
01215      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01216                                                                   ELGAOL  
01217      MOVE CMF-DESCR-LINE (1)  TO  WS-FROM-AGE-QUAL.               ELGAOL  
01218      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01219      MOVE WS-FROM-AGE-QUAL-PHRASE                                 ELGAOL  
01220               TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                 ELGAOL  
01221    0670-END.                                                      ELGAOL  
01222      EXIT.                                                        ELGAOL  
01223                                                                   ELGAOL  
01224                                                                   ELGAOL  
01225 /***********************************************************      ELGAOL  
01226 *                                                          *      ELGAOL  
01227 *        GET TO AGE PHRASE                                 *      ELGAOL  
01228 *                                                          *      ELGAOL  
01229 ************************************************************      ELGAOL  
01230  0680-GET-TO-AGE-PHRASE.                                          ELGAOL  
01231 *    *-----------------------------------------------------------*ELGAOL  
01232 *    *  PERFORMED BY 0660-GET-AGE-AND-REL-PHR.                   *ELGAOL  
01233 *    *-----------------------------------------------------------*ELGAOL  
01234      IF   ACCUM-AGE-LIMIT-TO-VAL = +999                           ELGAOL  
01235           MOVE PC-UNLIMITED TO  WS-TO-AGE-ALPHA                   ELGAOL  
01236      ELSE                                                         ELGAOL  
01237           MOVE ACCUM-AGE-LIMIT-TO-VAL TO WS-TO-AGE.               ELGAOL  
01238                                                                   ELGAOL  
01239      MOVE ACCUM-AGE-LIMIT-TO-IND    TO  CMF-CODE-VALUE.           ELGAOL  
01240      MOVE 'O-P-X-AGE-QUAL-IND-TO'   TO  CMF-ELEMENT-SYSTEM-NAME.  ELGAOL  
01241      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01242                                                                   ELGAOL  
01243      MOVE CMF-DESCR-LINE (1)  TO  WS-TO-AGE-QUAL.                 ELGAOL  
01244      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01245      MOVE WS-TO-AGE-QUAL-PHRASE                                   ELGAOL  
01246               TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                 ELGAOL  
01247    0680-END.                                                      ELGAOL  
01248      EXIT.                                                        ELGAOL  
01249                                                                   ELGAOL  
01250 ************************************************************      ELGAOL  
01251 *                                                          *      ELGAOL  
01252 *        MOVE TRANSLATION TO TCAR                          *      ELGAOL  
01253 *                                                          *      ELGAOL  
01254 ************************************************************      ELGAOL  
01255  0690-MOVE-TRANSLATION-TO-TCAR.                                   ELGAOL  
01256 *    *-----------------------------------------------------------*ELGAOL  
01257 *    *  PERFORMED BY 0520-GET-FYI-PHRASE,                        *ELGAOL  
01258 *    *      0560-GET-COST-CONTAINMENT-PHRA,                      *ELGAOL  
01259 *    *      0600-GET-CONDITION-BITS-PHRASE,                      *ELGAOL  
01260 *    *      0620-GET-PLACE-OF-TREATMENT-PH,                      *ELGAOL  
01261 *    *      0450-GET-L-O-B-PHRA.                                 *ELGAOL  
01262 *    *-----------------------------------------------------------*ELGAOL  
01263      PERFORM WITH TEST BEFORE                                     ELGAOL  
01264          VARYING CMF-DESCR-IDX FROM +1 BY +1                      ELGAOL  
01265               UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES           ELGAOL  
01266            IF TCAR-FROM-SUB >= 20                                 ELGAOL  
01267               PERFORM 0700-SEND-PART-PARA THRU 0700-END           ELGAOL  
01268            END-IF                                                 ELGAOL  
01269                 ADD +1 TO  TCAR-FROM-SUB                          ELGAOL  
01270                 MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)               ELGAOL  
01271                     TO  TCAR-FROM-LINE (TCAR-FROM-SUB)            ELGAOL  
01272      END-PERFORM.                                                 ELGAOL  
01273    0690-END.                                                      ELGAOL  
01274      EXIT.                                                        ELGAOL  
01275                                                                   ELGAOL  
01276                                                                   ELGAOL  
01277 /***********************************************************      ELGAOL  
01278 *                                                          *      ELGAOL  
01279 *        SEND PARTIAL PARAGRAPH                            *      ELGAOL  
01280 *                                                          *      ELGAOL  
01281 ************************************************************      ELGAOL  
01282  0700-SEND-PART-PARA.                                             ELGAOL  
01283 *    *-----------------------------------------------------------*ELGAOL  
01284 *    *  PERFORMED BY 0320 CREATE PCENT VALUE PHRASE              *ELGAOL  
01285 *    *               0640 MOVE TRANSLATION TO TCAR               *ELGAOL  
01286 *    *-----------------------------------------------------------*ELGAOL  
01287 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGAOL  
01288      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGAOL  
01289 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGAOL  
01290      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGAOL  
01291      MOVE +79                                                     ELGAOL  
01292        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGAOL  
01293           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGAOL  
01294           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGAOL  
01295           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGAOL  
01296           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGAOL  
01297           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGAOL  
01298           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGAOL  
01299           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGAOL  
01300           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGAOL  
01301           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGAOL  
01302 * -- UNSTRING/FLOW THE OUTPUT                                     ELGAOL  
01303      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGAOL  
01304 * -- MOVE FORMATTED TEXT TO OUTPUT ** EXCEPT LAST LINE **         ELGAOL  
01305      PERFORM WITH TEST BEFORE                                     ELGAOL  
01306            VARYING TCAR-X FROM 1 BY 1                             ELGAOL  
01307              UNTIL TCAR-X = TCAR-OUTPUT-FIELDS-USED               ELGAOL  
01308 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGAOL  
01309         IF COF-NBR-DTL-LINES NOT < 20                             ELGAOL  
01310         THEN                                                      ELGAOL  
01311            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGAOL  
01312            INITIALIZE COF-DTL                                     ELGAOL  
01313            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGAOL  
01314         END-IF                                                    ELGAOL  
01315 *    -- APPEND LINE TO OUTPUT                                     ELGAOL  
01316         ADD 1 TO COF-NBR-DTL-LINES                                ELGAOL  
01317         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGAOL  
01318           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGAOL  
01319      END-PERFORM.                                                 ELGAOL  
01320 * -- PUT LAST LINE OF COMPRESSED/UNSTRUNG OUTPUT INTO FROM AREA   ELGAOL  
01321      INITIALIZE TCAR-FROM-AREA                                    ELGAOL  
01322                 TCAR-FROM-LENGTH                                  ELGAOL  
01323                 TCAR-FROM-SUB.                                    ELGAOL  
01324      MOVE 1 TO TCAR-FROM-SUB.                                     ELGAOL  
01325      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELGAOL  
01326        TO TCAR-FROM-LINE (1).                                     ELGAOL  
01327    0700-END.                                                      ELGAOL  
01328      EXIT.                                                        ELGAOL  
01329                                                                   ELGAOL  
01330                                                                   ELGAOL  
01331 /***********************************************************      ELGAOL  
01332 *                                                          *      ELGAOL  
01333 *        COMPLETE AND SEND PARAGRAPH                       *      ELGAOL  
01334 *                                                          *      ELGAOL  
01335 ************************************************************      ELGAOL  
01336  0710-COMPLETE-AND-SEND-PARA.                                     ELGAOL  
01337 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGAOL  
01338      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGAOL  
01339 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGAOL  
01340      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGAOL  
01341      MOVE +79                                                     ELGAOL  
01342        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGAOL  
01343           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGAOL  
01344           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGAOL  
01345           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGAOL  
01346           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGAOL  
01347           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGAOL  
01348           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGAOL  
01349           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGAOL  
01350           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGAOL  
01351           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGAOL  
01352 * -- UNSTRING/FLOW THE OUTPUT                                     ELGAOL  
01353      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGAOL  
01354 * -- MOVE FORMATTED TEXT TO OUTPUT                                ELGAOL  
01355      PERFORM WITH TEST BEFORE                                     ELGAOL  
01356            VARYING TCAR-X FROM 1 BY 1                             ELGAOL  
01357              UNTIL TCAR-X > TCAR-OUTPUT-FIELDS-USED               ELGAOL  
01358 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGAOL  
01359         IF COF-NBR-DTL-LINES >= 20                                ELGAOL  
01360         THEN                                                      ELGAOL  
01361            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGAOL  
01362            INITIALIZE COF-DTL                                     ELGAOL  
01363            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGAOL  
01364         END-IF                                                    ELGAOL  
01365 *    -- APPEND LINE TO OUTPUT                                     ELGAOL  
01366         ADD 1 TO COF-NBR-DTL-LINES                                ELGAOL  
01367         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGAOL  
01368           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGAOL  
01369      END-PERFORM.                                                 ELGAOL  
01370                                                                   ELGAOL  
01371 * -- CLEAR THE COMPRESSION WORK AREA                              ELGAOL  
01372      INITIALIZE TCAR-FROM-AREA                                    ELGAOL  
01373                 TCAR-FROM-SUB.                                    ELGAOL  
01374    0710-END.                                                      ELGAOL  
01375      EXIT.                                                        ELGAOL  
01376                                                                   ELGAOL  
01377                                                                   ELGAOL  
01378                                                                   ELGAOL  
01379 /***********************************************************      ELGAOL  
01380 *                                                          *      ELGAOL  
01381 *        INSERT A BLANK LINE                               *      ELGAOL  
01382 *                                                          *      ELGAOL  
01383 ************************************************************      ELGAOL  
01384  0720-INSERT-BLANK-LINE.                                          ELGAOL  
01385      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGAOL  
01386      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELGAOL  
01387 * -- CALL THE OUTPUT MODULE                                       ELGAOL  
01388      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA.                  ELGAOL  
01389      INITIALIZE COF-DTL.                                          ELGAOL  
01390      MOVE ZERO TO COF-NBR-DTL-LINES.                              ELGAOL  
01391    0720-END.                                                      ELGAOL  
01392      EXIT.                                                        ELGAOL  
01393                                                                   ELGAOL  
01394                                                                   ELGAOL  
01395                                                                   ELGAOL  
01396 ************************************************************      ELGAOL  
01397 *                                                          *      ELGAOL  
01398 *        INITIALIZE INTERNAL SWITCHES                      *      ELGAOL  
01399 *                                                          *      ELGAOL  
01400 ************************************************************      ELGAOL  
01401  0730-INITIALIZE-SWITCHES.                                        ELGAOL  
01402 *    *-----------------------------------------------------------*ELGAOL  
01403 *    *  PERFORMED BY 0270-PROCESS-PERCENT-PHRASE.                *ELGAOL  
01404 *    *-----------------------------------------------------------*ELGAOL  
01405      SET WS-HAS-NO-IBGR                                           ELGAOL  
01406          WS-HAS-NO-IDGD                                           ELGAOL  
01407          WS-HAS-NO-IPGN                                           ELGAOL  
01408          WS-HAS-NO-IPGP                                           ELGAOL  
01409          WS-HAS-NO-IPGT                                           ELGAOL  
01410          WS-HAS-NO-IPGS                                           ELGAOL  
01411          WS-HAS-NO-INTERNAL TO TRUE.                              ELGAOL  
01412    0730-END.                                                      ELGAOL  
01413      EXIT.                                                        ELGAOL  
01414                                                                   ELGAOL  
01415                                                                   ELGAOL  
01416 /***********************************************************      ELGAOL  
01417 *                                                          *      ELGAOL  
01418 *        INITIALIZE COMPRESS AREA                          *      ELGAOL  
01419 *                                                          *      ELGAOL  
01420 ************************************************************      ELGAOL  
01421  0740-INITIALIZE-COMPRESS-AREA.                                   ELGAOL  
01422 *    *-----------------------------------------------------------*ELGAOL  
01423 *    *  PERFORMED BY 0270-PROCESS-PERCENT-PHRASE                 *ELGAOL  
01424 *    *      0310-CREATE-MULTI-PCENT-PHRASE.                      *ELGAOL  
01425 *    *-----------------------------------------------------------*ELGAOL  
01426      INITIALIZE TCAR-FROM-AREA                                    ELGAOL  
01427                 TCAR-FROM-LENGTH                                  ELGAOL  
01428                 TCAR-FROM-SUB.                                    ELGAOL  
01429    0740-END.                                                      ELGAOL  
01430      EXIT.                                                        ELGAOL  
01431                                                                   ELGAOL  
01432                                                                   ELGAOL  
01433 /***********************************************************      ELGAOL  
01434 *                                                          *      ELGAOL  
01435 *        CHECK FOR INTERNALS                               *      ELGAOL  
01436 *                                                          *      ELGAOL  
01437 ************************************************************      ELGAOL  
01438  0750-CHECK-FOR-INTERNALS.                                        ELGAOL  
01439 *    *-----------------------------------------------------------*ELGAOL  
01440 *    *  PERFORMED BY 0340-PROCESS-VALUE-LIMIT                    *ELGAOL  
01441 *    *-----------------------------------------------------------*ELGAOL  
01442                                                                   ELGAOL  
01443      IF  NO-IBGR-SLOT-NBR (ASC-DES-INDEX)                         ELGAOL  
01444          CONTINUE                                                 ELGAOL  
01445      ELSE                                                         ELGAOL  
01446          SET WS-HAS-IBGR                                          ELGAOL  
01447              WS-HAS-AN-INTERNAL TO TRUE.                          ELGAOL  
01448                                                                   ELGAOL  
01449      IF  NO-IDGD-SLOT-NBR (ASC-DES-INDEX)                         ELGAOL  
01450          CONTINUE                                                 ELGAOL  
01451      ELSE                                                         ELGAOL  
01452          SET WS-HAS-IDGD                                          ELGAOL  
01453              WS-HAS-AN-INTERNAL TO TRUE.                          ELGAOL  
01454                                                                   ELGAOL  
01455      IF  NO-IPGN-SLOT-NBR (ASC-DES-INDEX)                         ELGAOL  
01456          CONTINUE                                                 ELGAOL  
01457      ELSE                                                         ELGAOL  
01458          SET WS-HAS-IPGN                                          ELGAOL  
01459              WS-HAS-AN-INTERNAL TO TRUE.                          ELGAOL  
01460                                                                   ELGAOL  
01461      IF  NO-IPGP-SLOT-NBR (ASC-DES-INDEX)                         ELGAOL  
01462          CONTINUE                                                 ELGAOL  
01463      ELSE                                                         ELGAOL  
01464          SET WS-HAS-IPGP                                          ELGAOL  
01465              WS-HAS-AN-INTERNAL TO TRUE.                          ELGAOL  
01466                                                                   ELGAOL  
01467      IF  NO-IPGT-SLOT-NBR (ASC-DES-INDEX)                         ELGAOL  
01468          CONTINUE                                                 ELGAOL  
01469      ELSE                                                         ELGAOL  
01470          SET WS-HAS-IPGT                                          ELGAOL  
01471              WS-HAS-AN-INTERNAL TO TRUE.                          ELGAOL  
01472                                                                   ELGAOL  
01473      IF  ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX) NOT NUMERIC          ELGAOL  
01474        MOVE ZERO TO ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX)           ELGAOL  
01475      END-IF.                                                      ELGAOL  
01476                                                                   ELGAOL  
01477      IF  NO-IPGS-SLOT-NBR (ASC-DES-INDEX)                         ELGAOL  
01478          CONTINUE                                                 ELGAOL  
01479      ELSE                                                         ELGAOL  
01480          SET WS-HAS-IPGS                                          ELGAOL  
01481              WS-HAS-AN-INTERNAL TO TRUE.                          ELGAOL  
01482                                                                   ELGAOL  
01483    0750-END.                                                      ELGAOL  
01484      EXIT.                                                        ELGAOL  
01485                                                                   ELGAOL  
01486                                                                   ELGAOL  
01487 /***********************************************************      ELGAOL  
01488 *                                                          *      ELGAOL  
01489 *        GET INTERNAL TABULARS LIST                        *      ELGAOL  
01490 *                                                          *      ELGAOL  
01491 ************************************************************      ELGAOL  
01492  0760-GET-INTERNAL-TABULARS-LIS.                                  ELGAOL  
01493 *    *-----------------------------------------------------------*ELGAOL  
01494 *    *  PERFORMED BY 0500-CREATE-O-P-X-APPLIC                    *ELGAOL  
01495 *    *-----------------------------------------------------------*ELGAOL  
01496      INITIALIZE WS-INT-TAB-LST                                    ELGAOL  
01497                 WS-INT-TAB-TXT                                    ELGAOL  
01498                 WS-TAB-SUB.                                       ELGAOL  
01499                                                                   ELGAOL  
01500      IF WS-HAS-IBGR                                               ELGAOL  
01501         ADD 1 TO WS-TAB-SUB                                       ELGAOL  
01502         SET WS-INT-TAB-IBGR (WS-TAB-SUB)                          ELGAOL  
01503             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGAOL  
01504                                                                   ELGAOL  
01505      IF WS-HAS-IDGD                                               ELGAOL  
01506         ADD 1 TO WS-TAB-SUB                                       ELGAOL  
01507         SET WS-INT-TAB-IDGD (WS-TAB-SUB)                          ELGAOL  
01508             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGAOL  
01509                                                                   ELGAOL  
01510      IF WS-HAS-IPGN                                               ELGAOL  
01511         ADD 1 TO WS-TAB-SUB                                       ELGAOL  
01512         SET WS-INT-TAB-IPGN (WS-TAB-SUB)                          ELGAOL  
01513             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGAOL  
01514                                                                   ELGAOL  
01515      IF WS-HAS-IPGP                                               ELGAOL  
01516         ADD 1 TO WS-TAB-SUB                                       ELGAOL  
01517         SET WS-INT-TAB-IPGP (WS-TAB-SUB)                          ELGAOL  
01518             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGAOL  
01519                                                                   ELGAOL  
01520      IF WS-HAS-IPGT                                               ELGAOL  
01521         ADD 1 TO WS-TAB-SUB                                       ELGAOL  
01522         SET WS-INT-TAB-IPGT (WS-TAB-SUB)                          ELGAOL  
01523             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGAOL  
01524                                                                   ELGAOL  
01525      IF WS-HAS-IPGS                                               ELGAOL  
01526         ADD 1 TO WS-TAB-SUB                                       ELGAOL  
01527         SET WS-INT-TAB-IPGS (WS-TAB-SUB)                          ELGAOL  
01528             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGAOL  
01529                                                                   ELGAOL  
01530                                                                   ELGAOL  
01531      IF WS-TAB-SUB  >  0                                          ELGAOL  
01532         SET WS-INT-TAB-END (WS-TAB-SUB) TO TRUE                   ELGAOL  
01533         IF WS-TAB-SUB  >  1                                       ELGAOL  
01534            SET WS-INT-TAB-AND (WS-TAB-SUB - 1) TO TRUE            ELGAOL  
01535         END-IF                                                    ELGAOL  
01536         ADD  +1  TO  TCAR-FROM-SUB                                ELGAOL  
01537         MOVE PC-SUBJECT-TO     TO  TCAR-FROM-LINE (TCAR-FROM-SUB) ELGAOL  
01538                                                                   ELGAOL  
01539         ADD  +1  TO  TCAR-FROM-SUB                                ELGAOL  
01540         MOVE WS-INT-TAB-TXT-1  TO TCAR-FROM-LINE  (TCAR-FROM-SUB) ELGAOL  
01541                                                                   ELGAOL  
01542         IF WS-TAB-SUB > 3                                         ELGAOL  
01543         THEN                                                      ELGAOL  
01544            ADD 1 TO TCAR-FROM-SUB                                 ELGAOL  
01545            MOVE WS-INT-TAB-TXT-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB)ELGAOL  
01546         END-IF                                                    ELGAOL  
01547         ADD 1 TO TCAR-FROM-SUB                                    ELGAOL  
01548         MOVE PC-CONSIDERATIONS TO  TCAR-FROM-LINE (TCAR-FROM-SUB) ELGAOL  
01549      END-IF.                                                      ELGAOL  
01550    0760-END.                                                      ELGAOL  
01551      EXIT.                                                        ELGAOL  
01552                                                                   ELGAOL  
01553 /***********************************************************      ELGAOL  
01554 *                                                          *      ELGAOL  
01555 *        CREATE DEFINITION SENTENCE                        *      ELGAOL  
01556 *                                                          *      ELGAOL  
01557 ************************************************************      ELGAOL  
01558  0800-CREATE-DEFINITION-SENTENC.                                  ELGAOL  
01559 *    *-----------------------------------------------------------*ELGAOL  
01560 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGAOL  
01561 *    *-----------------------------------------------------------*ELGAOL  
01562      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01563      MOVE WS-DEFINITION-LINE                                      ELGAOL  
01564           TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                     ELGAOL  
01565      MOVE ACCUM-DEFINITION   TO  CMF-CODE-VALUE.                  ELGAOL  
01566      MOVE 'O-P-X-DEFINITION' TO  CMF-ELEMENT-SYSTEM-NAME.         ELGAOL  
01567      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01568      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
01569      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01570      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGAOL  
01571      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGAOL  
01572      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGAOL  
01573    0800-END.                                                      ELGAOL  
01574      EXIT.                                                        ELGAOL  
01575                                                                   ELGAOL  
01576                                                                   ELGAOL  
01577 /***********************************************************      ELGAOL  
01578 *                                                          *      ELGAOL  
01579 *        CREATE BENEFIT PERIOD SENTENCE                    *      ELGAOL  
01580 *                                                          *      ELGAOL  
01581 ************************************************************      ELGAOL  
01582  0830-CREATE-BENEFIT-PERIOD-SEN.                                  ELGAOL  
01583 *    *-----------------------------------------------------------*ELGAOL  
01584 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGAOL  
01585 *    *-----------------------------------------------------------*ELGAOL  
01586      IF TCAR-FROM-SUB > 1                                         ELGAOL  
01587         PERFORM 0700-SEND-PART-PARA THRU 0700-END.                ELGAOL  
01588      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01589      MOVE WS-BEN-PER-PHRASE  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGAOL  
01590      MOVE ACCUM-BENEFIT-PERIOD   TO  CMF-CODE-VALUE.              ELGAOL  
01591      MOVE 'O-P-X-BENEFIT-PERIOD' TO  CMF-ELEMENT-SYSTEM-NAME.     ELGAOL  
01592      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01593      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
01594      IF ACCUM-BEN-PER-TIME-FCTR NOT = ZEROS                       ELGAOL  
01595          PERFORM 0880-GET-BP-TIME-QUAL-FCTR-PHR THRU 0880-END.    ELGAOL  
01596      IF ACCUM-INTERVAL-TIME-FCTR NOT = ZEROS                      ELGAOL  
01597          PERFORM 0900-GET-INTV-TIME-QUAL-FTR-PH THRU 0900-END.    ELGAOL  
01598      IF ACCUM-INTERVAL-OVRD-IND NOT = ZEROS                       ELGAOL  
01599          PERFORM 0920-CREATE-INTERVAL-OVERRIDE THRU 0920-END.     ELGAOL  
01600      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01601      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGAOL  
01602      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGAOL  
01603      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGAOL  
01604    0830-END.                                                      ELGAOL  
01605      EXIT.                                                        ELGAOL  
01606                                                                   ELGAOL  
01607                                                                   ELGAOL  
01608 /***********************************************************      ELGAOL  
01609 *                                                          *      ELGAOL  
01610 *        CREATE CARRY OVER CREDIT SENTENCE                 *      ELGAOL  
01611 *                                                          *      ELGAOL  
01612 ************************************************************      ELGAOL  
01613  0840-CREATE-CARRY-OVR.                                           ELGAOL  
01614 *    *-----------------------------------------------------------*ELGAOL  
01615 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGAOL  
01616 *    *-----------------------------------------------------------*ELGAOL  
01617      IF TCAR-FROM-SUB > 1                                         ELGAOL  
01618         PERFORM 0700-SEND-PART-PARA THRU 0700-END.                ELGAOL  
01619      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01620      MOVE CARRY-OVER-MSG TO TCAR-FROM-LINE                        ELGAOL  
01621          (TCAR-FROM-SUB).                                         ELGAOL  
01622      MOVE ACCUM-CARRY-OVER-CREDIT-IND  TO  CMF-CODE-VALUE.        ELGAOL  
01623      MOVE      'CARRY-OVER-CREDIT-IND' TO  CMF-ELEMENT-SYSTEM-NAMEELGAOL  
01624      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01625      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGAOL  
01626      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01627      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGAOL  
01628      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGAOL  
01629      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGAOL  
01630    0840-END.                                                      ELGAOL  
01631      EXIT.                                                        ELGAOL  
01632                                                                   ELGAOL  
01633                                                                   ELGAOL  
01634 /***********************************************************      ELGAOL  
01635 *                                                          *      ELGAOL  
01636 *        CALL INTERNALS                                    *      ELGAOL  
01637 *                                                          *      ELGAOL  
01638 ************************************************************      ELGAOL  
01639  0850-CALL-INTERNALS.                                             ELGAOL  
01640 *    *-----------------------------------------------------------*ELGAOL  
01641 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGAOL  
01642 *    *-----------------------------------------------------------*ELGAOL  
01643      IF WS-HAS-IBGR                                               ELGAOL  
01644         MOVE PC-IBGR             TO  SRP-INTERNAL-TAB-ID          ELGAOL  
01645         EXEC CICS LINK PROGRAM ('ELGIBGR')                        ELGAOL  
01646                        COMMAREA (DFHCOMMAREA)                     ELGAOL  
01647                        END-EXEC.                                  ELGAOL  
01648                                                                   ELGAOL  
01649      IF WS-HAS-IDGD                                               ELGAOL  
01650         MOVE PC-IDGD             TO  SRP-INTERNAL-TAB-ID          ELGAOL  
01651         EXEC CICS LINK PROGRAM ('ELGIDGD')                        ELGAOL  
01652                        COMMAREA (DFHCOMMAREA)                     ELGAOL  
01653                        END-EXEC.                                  ELGAOL  
01654                                                                   ELGAOL  
01655      IF WS-HAS-IPGP                                               ELGAOL  
01656         MOVE PC-IPGP             TO  SRP-INTERNAL-TAB-ID          ELGAOL  
01657         EXEC CICS LINK PROGRAM ('ELGIPGP')                        ELGAOL  
01658                        COMMAREA (DFHCOMMAREA)                     ELGAOL  
01659                        END-EXEC.                                  ELGAOL  
01660                                                                   ELGAOL  
01661      IF WS-HAS-IPGN                                               ELGAOL  
01662         MOVE PC-IPGN             TO  SRP-INTERNAL-TAB-ID          ELGAOL  
01663         EXEC CICS LINK PROGRAM ('ELGIPGN')                        ELGAOL  
01664                        COMMAREA (DFHCOMMAREA)                     ELGAOL  
01665                        END-EXEC.                                  ELGAOL  
01666                                                                   ELGAOL  
01667      IF WS-HAS-IPGT                                               ELGAOL  
01668         MOVE PC-IPGT             TO  SRP-INTERNAL-TAB-ID          ELGAOL  
01669         EXEC CICS LINK PROGRAM ('ELGIPGT')                        ELGAOL  
01670                        COMMAREA (DFHCOMMAREA)                     ELGAOL  
01671                        END-EXEC.                                  ELGAOL  
01672      IF WS-HAS-IPGS                                               ELGAOL  
01673         MOVE PC-IPGS             TO  SRP-INTERNAL-TAB-ID          ELGAOL  
01674         EXEC CICS LINK PROGRAM ('ELGIPGS')                        ELGAOL  
01675                        COMMAREA (DFHCOMMAREA)                     ELGAOL  
01676                        END-EXEC.                                  ELGAOL  
01677    0850-END.                                                      ELGAOL  
01678      EXIT.                                                        ELGAOL  
01679                                                                   ELGAOL  
01680                                                                   ELGAOL  
01681 /***********************************************************      ELGAOL  
01682 *                                                          *      ELGAOL  
01683 *        GET BP TIME QUAL FCTR PHRASE                      *      ELGAOL  
01684 *                                                          *      ELGAOL  
01685 ************************************************************      ELGAOL  
01686  0880-GET-BP-TIME-QUAL-FCTR-PHR.                                  ELGAOL  
01687 *    *-----------------------------------------------------------*ELGAOL  
01688 *    *  PERFORMED BY 0830-CREATE-BENEFIT-PERIOD-SEN.             *ELGAOL  
01689 *    *-----------------------------------------------------------*ELGAOL  
01690      MOVE ACCUM-BEN-PER-TIME-FCTR  TO  WS-BP-TIME-FCTR.           ELGAOL  
01691      MOVE ACCUM-BEN-PER-TIME-QUAL   TO  CMF-CODE-VALUE.           ELGAOL  
01692      MOVE 'O-P-X-BEN-PER-TIME-QUAL' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGAOL  
01693      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01694      MOVE CMF-DESCR-LINE (1)  TO  WS-BP-TIME-QUAL.                ELGAOL  
01695      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01696      MOVE WS-BP-TIME-FCTR-QUAL-PHRASE TO                          ELGAOL  
01697           TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGAOL  
01698    0880-END.                                                      ELGAOL  
01699      EXIT.                                                        ELGAOL  
01700                                                                   ELGAOL  
01701                                                                   ELGAOL  
01702                                                                   ELGAOL  
01703                                                                   ELGAOL  
01704 /***********************************************************      ELGAOL  
01705 *                                                          *      ELGAOL  
01706 *        GET INTV TIME QUAL FTR PH                         *      ELGAOL  
01707 *                                                          *      ELGAOL  
01708 ************************************************************      ELGAOL  
01709  0900-GET-INTV-TIME-QUAL-FTR-PH.                                  ELGAOL  
01710 *    *-----------------------------------------------------------*ELGAOL  
01711 *    *  PERFORMED BY 0830-CREATE-BENEFIT-PERIOD-SEN.             *ELGAOL  
01712 *    *-----------------------------------------------------------*ELGAOL  
01713      MOVE ACCUM-INTERVAL-TIME-FCTR  TO  WS-INTERVAL-TIME-FCTR.    ELGAOL  
01714      MOVE ACCUM-INTERVAL-TIME-FCTR  TO  CMF-CODE-VALUE.           ELGAOL  
01715      MOVE 'O-P-X-INTERVAL-TIME-QUAL' TO  CMF-ELEMENT-SYSTEM-NAME. ELGAOL  
01716      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01717      MOVE CMF-DESCR-LINE (1)  TO  WS-INTERVAL-TIME-QUAL.          ELGAOL  
01718      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGAOL  
01719      MOVE WS-INTERVAL-TIME-FCTR-QUAL-PHR                          ELGAOL  
01720               TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                 ELGAOL  
01721    0900-END.                                                      ELGAOL  
01722      EXIT.                                                        ELGAOL  
01723                                                                   ELGAOL  
01724                                                                   ELGAOL  
01725 /***********************************************************      ELGAOL  
01726 *                                                          *      ELGAOL  
01727 *        CREATE INTERVAL OVERRIDE NOTE                     *      ELGAOL  
01728 *                                                          *      ELGAOL  
01729 ************************************************************      ELGAOL  
01730  0920-CREATE-INTERVAL-OVERRIDE.                                   ELGAOL  
01731 *    *-----------------------------------------------------------*ELGAOL  
01732 *    *  PERFORMED BY 0830-CREATE-BENEFIT-PERIOD-SEN.             *ELGAOL  
01733 *    *-----------------------------------------------------------*ELGAOL  
01734      ADD +1 TO TCAR-FROM-SUB.                                     ELGAOL  
01735      MOVE PC-NOTE  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGAOL  
01736      PERFORM 0930-CREATE-INTVL-OVERRIDE-PHR THRU 0930-END.        ELGAOL  
01737      ADD +1 TO TCAR-FROM-SUB.                                     ELGAOL  
01738      MOVE ')'      TO  TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGAOL  
01739    0920-END.                                                      ELGAOL  
01740      EXIT.                                                        ELGAOL  
01741                                                                   ELGAOL  
01742                                                                   ELGAOL  
01743 ************************************************************      ELGAOL  
01744 *                                                          *      ELGAOL  
01745 *        CREATE INTERVAL OVERRIDE PHRASE                   *      ELGAOL  
01746 *                                                          *      ELGAOL  
01747 ************************************************************      ELGAOL  
01748  0930-CREATE-INTVL-OVERRIDE-PHR.                                  ELGAOL  
01749 *    *-----------------------------------------------------------*ELGAOL  
01750 *    *  PERFORMED BY 0920-CREATE-INTERVAL-OVERRIDE               *ELGAOL  
01751 *    *-----------------------------------------------------------*ELGAOL  
01752      ADD +1 TO TCAR-FROM-SUB.                                     ELGAOL  
01753      IF ACCUM-INTERVAL-OVRD-IND = '1'                             ELGAOL  
01754         PERFORM 0931-CREATE-OVERRIDE-PHR-1 THRU 0931-END          ELGAOL  
01755      ELSE IF ACCUM-INTERVAL-OVRD-IND = '2'                        ELGAOL  
01756              PERFORM 0932-CREATE-OVERRIDE-PHR-2 THRU 0932-END     ELGAOL  
01757           ELSE IF ACCUM-INTERVAL-OVRD-IND = '3'                   ELGAOL  
01758                   PERFORM 0933-CREATE-OVERRIDE-PHR-3              ELGAOL  
01759                      THRU 0933-END                                ELGAOL  
01760                ELSE PERFORM 0940-TRANSLATE-OVERRIDE-IND THRU      ELGAOL  
01761                             0940-END.                             ELGAOL  
01762    0930-END.                                                      ELGAOL  
01763      EXIT.                                                        ELGAOL  
01764                                                                   ELGAOL  
01765                                                                   ELGAOL  
01766 /***********************************************************      ELGAOL  
01767 *                                                          *      ELGAOL  
01768 *        CREATE INTERVAL OVERRIDE PHRASE 1                 *      ELGAOL  
01769 *                                                          *      ELGAOL  
01770 ************************************************************      ELGAOL  
01771 *    *-----------------------------------------------------------*ELGAOL  
01772 *    *  PERFORMED BY 0930-CREATE-INTVL-OVERRIDE-PHR              *ELGAOL  
01773 *    *-----------------------------------------------------------*ELGAOL  
01774  0931-CREATE-OVERRIDE-PHR-1.                                      ELGAOL  
01775      MOVE ACCUM-INTERVAL-OVRD-VALUE TO                            ELGAOL  
01776           WS-INTERVAL-OVERRIDE-VALUE-1.                           ELGAOL  
01777      MOVE WS-INTERVAL-OVERRIDE-PHRASE-1 TO                        ELGAOL  
01778           TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGAOL  
01779    0931-END.                                                      ELGAOL  
01780      EXIT.                                                        ELGAOL  
01781                                                                   ELGAOL  
01782                                                                   ELGAOL  
01783 ************************************************************      ELGAOL  
01784 *                                                          *      ELGAOL  
01785 *        CREATE INTERVAL OVERRIDE PHRASE 2                 *      ELGAOL  
01786 *                                                          *      ELGAOL  
01787 ************************************************************      ELGAOL  
01788 *    *-----------------------------------------------------------*ELGAOL  
01789 *    *  PERFORMED BY 0930-CREATE-INTVL-OVERRIDE-PHR              *ELGAOL  
01790 *    *-----------------------------------------------------------*ELGAOL  
01791  0932-CREATE-OVERRIDE-PHR-2.                                      ELGAOL  
01792      MOVE ACCUM-INTERVAL-OVRD-VALUE TO                            ELGAOL  
01793           WS-INTERVAL-OVERRIDE-VALUE-2.                           ELGAOL  
01794      MOVE WS-INTERVAL-OVERRIDE-PHRASE-2 TO                        ELGAOL  
01795           TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGAOL  
01796    0932-END.                                                      ELGAOL  
01797      EXIT.                                                        ELGAOL  
01798                                                                   ELGAOL  
01799                                                                   ELGAOL  
01800 ************************************************************      ELGAOL  
01801 *                                                          *      ELGAOL  
01802 *        CREATE INTERVAL OVERRIDE PHRASE 3                 *      ELGAOL  
01803 *                                                          *      ELGAOL  
01804 ************************************************************      ELGAOL  
01805 *    *-----------------------------------------------------------*ELGAOL  
01806 *    *  PERFORMED BY 0930-CREATE-INTVL-OVERRIDE-PHR              *ELGAOL  
01807 *    *-----------------------------------------------------------*ELGAOL  
01808  0933-CREATE-OVERRIDE-PHR-3.                                      ELGAOL  
01809      MOVE ACCUM-INTERVAL-OVRD-VALUE TO                            ELGAOL  
01810           WS-INTERVAL-OVERRIDE-VALUE-3.                           ELGAOL  
01811      MOVE WS-INTERVAL-OVERRIDE-PHRASE-3 TO                        ELGAOL  
01812           TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGAOL  
01813    0933-END.                                                      ELGAOL  
01814      EXIT.                                                        ELGAOL  
01815                                                                   ELGAOL  
01816                                                                   ELGAOL  
01817 /***********************************************************      ELGAOL  
01818 *                                                          *      ELGAOL  
01819 *        TRANSLATE OVERRIDE INDICATOR                      *      ELGAOL  
01820 *                                                          *      ELGAOL  
01821 ************************************************************      ELGAOL  
01822 *    *-----------------------------------------------------------*ELGAOL  
01823 *    *  PERFORMED BY 0930-CREATE-INTVL-OVERRIDE-PHR              *ELGAOL  
01824 *    *-----------------------------------------------------------*ELGAOL  
01825  0940-TRANSLATE-OVERRIDE-IND.                                     ELGAOL  
01826      MOVE ACCUM-INTERVAL-OVRD-IND  TO  CMF-CODE-VALUE.            ELGAOL  
01827      MOVE 'O-P-X-INTERVAL-OVRD-IND' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGAOL  
01828      PERFORM 0950-LINK-TO-CODES-MANUAL-FORX THRU 0950-END.        ELGAOL  
01829    0940-END.                                                      ELGAOL  
01830      EXIT.                                                        ELGAOL  
01831                                                                   ELGAOL  
01832                                                                   ELGAOL  
01833 /***********************************************************      ELGAOL  
01834 *                                                          *      ELGAOL  
01835 *        LINK TO CODES MANUAL FOR TRANSLATION              *      ELGAOL  
01836 *                                                          *      ELGAOL  
01837 ************************************************************      ELGAOL  
01838  0950-LINK-TO-CODES-MANUAL-FORX.                                  ELGAOL  
01839 *    *-----------------------------------------------------------*ELGAOL  
01840 *    *  PERFORMED BY 0530-TRANSLATE-FYI-VALUE,                   *ELGAOL  
01841 *    *      0570-TRANSLATE-COST-CONTAINMEN,                      *ELGAOL  
01842 *    *      0630-TRANSLATE-PLACE-OF-TREATM,                      *ELGAOL  
01843 *    *      0650-TRANSLATE-RELATIONSHIP,                         *ELGAOL  
01844 *    *      0670-GET-FROM-AGE-PHRASE,                            *ELGAOL  
01845 *    *      0680-GET-TO-AGE-PHRASE,                               ELGAOL  
01846 *    *      0440-TRANSLATE-FAMILY-OR-INDIV,                      *ELGAOL  
01847 *    *      0460-TRANSLATE-LINE-OF-BUSINES,                      *ELGAOL  
01848 *    *      0810-TRANSLATE-DEFINITION-INDI,                      *ELGAOL  
01849 *    *      0860-TRANSLATE-BENEFIT-PERIOD,                       *ELGAOL  
01850 *    *      0390-TRANSLATE-VALUE-QUALIFIER,                      *ELGAOL  
01851 *    *      0890-TRANSLATE-BEN-PER-TIME-QU.                      *ELGAOL  
01852 *    *-----------------------------------------------------------*ELGAOL  
01853      MOVE PC-AOL  TO  CMF-RECORD-PREFIX.                          ELGAOL  
01854      EXEC CICS LINK PROGRAM('ELUCMIF')                            ELGAOL  
01855                     COMMAREA(DFHCOMMAREA)                         ELGAOL  
01856                     LENGTH(LENGTH OF DFHCOMMAREA)                 ELGAOL  
01857            END-EXEC.                                              ELGAOL  
01858      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGAOL  
01859      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGAOL  
01860              ADDRESS OF CMF-DESCR.                                ELGAOL  
01861    0950-END.                                                      ELGAOL  
01862      EXIT.                                                        ELGAOL  
01863                                                                   ELGAOL  
01864                                                                   ELGAOL  
01865                                                                   ELGAOL  
01866                                                                   ELGAOL  
01867 /***********************************************************      ELGAOL  
01868 *                                                          *      ELGAOL  
01869 *        DISPLAY OUT-OF-POCKET END SENTENCE                *      ELGAOL  
01870 *                                                          *      ELGAOL  
01871 ************************************************************      ELGAOL  
01872  0960-DISPLAY-O-P-X-END-SENTNCE.                                  ELGAOL  
01873 *    *-----------------------------------------------------------*ELGAOL  
01874 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGAOL  
01875 *    *-----------------------------------------------------------*ELGAOL  
01876      MOVE +2                  TO  COF-NBR-DTL-LINES.              ELGAOL  
01877      MOVE OUT-OF-POCKET-END-MSG-1A TO COF-DTL-LINE (1).           ELGAOL  
01878      MOVE OUT-OF-POCKET-END-MSG-1B TO COF-DTL-LINE (2).           ELGAOL  
01879      PERFORM 1300-LINK-TO-OUTPUT-MODULE THRU 1300-END.            ELGAOL  
01880    0960-END.                                                      ELGAOL  
01881      EXIT.                                                        ELGAOL  
01882                                                                   ELGAOL  
01883                                                                   ELGAOL  
01884 ************************************************************      ELGAOL  
01885 *                                                          *      ELGAOL  
01886 *        SETUP AND READ FILE                               *      ELGAOL  
01887 *                                                          *      ELGAOL  
01888 ************************************************************      ELGAOL  
01889  0970-SETUP-AND-READ-FILE.                                        ELGAOL  
01890 *    *-----------------------------------------------------------*ELGAOL  
01891 *    *  PERFORMED BY 0250-DISPLAY-REGULAR-TEXT,                  *ELGAOL  
01892 *    *      0260-DISPLAY-OCCURENCE-TEXT.                         *ELGAOL  
01893 *    *-----------------------------------------------------------*ELGAOL  
01894      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGAOL  
01895      SET IOP-FCQ-NONE         TO  TRUE.                           ELGAOL  
01896      SET IOP-KVQ-NONE         TO  TRUE.                           ELGAOL  
01897      SET IOP-STG-MODE-LOCATE  TO  TRUE.                           ELGAOL  
01898      CALL 'ELUIOPGM' USING DFHEIBLK                               ELGAOL  
01899                            DFHCOMMAREA                            ELGAOL  
01900                            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS ELGAOL  
01901                            END-CALL.                              ELGAOL  
01902      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY                       ELGAOL  
01903                               TO  IOP-REC-PTR.                    ELGAOL  
01904    0970-END.                                                      ELGAOL  
01905      EXIT.                                                        ELGAOL  
01906                                                                   ELGAOL  
01907                                                                   ELGAOL  
01908 /***********************************************************      ELGAOL  
01909 *                                                          *      ELGAOL  
01910 *        OBTAIN TOPIC HEADER                               *      ELGAOL  
01911 *                                                          *      ELGAOL  
01912 ************************************************************      ELGAOL  
01913  0980-OBTAIN-TOPIC-HEADER.                                        ELGAOL  
01914 *    *-----------------------------------------------------------*ELGAOL  
01915 *    *  PERFORMED BY 0180-PROCESS, 0260-DISPLAY-OCCURENCE-TEXT.  *ELGAOL  
01916 *    *-----------------------------------------------------------*ELGAOL  
01917      MOVE +2            TO  COF-NBR-HDR-LINES.                    ELGAOL  
01918      MOVE TOPIC-HEADER  TO  COF-HDR-LINE (2).                     ELGAOL  
01919      MOVE  0            TO  COF-NBR-DTL-LINES.                    ELGAOL  
01920      SET  COF-NEW-PAGE  TO  TRUE.                                 ELGAOL  
01921      PERFORM 0990-SETUP-FOR-OUTPUT-LINK THRU 0990-END.            ELGAOL  
01922    0980-END.                                                      ELGAOL  
01923      EXIT.                                                        ELGAOL  
01924                                                                   ELGAOL  
01925                                                                   ELGAOL  
01926 ************************************************************      ELGAOL  
01927 *                                                          *      ELGAOL  
01928 *        SETUP FOR OUTPUT LINK                             *      ELGAOL  
01929 *                                                          *      ELGAOL  
01930 ************************************************************      ELGAOL  
01931  0990-SETUP-FOR-OUTPUT-LINK.                                      ELGAOL  
01932 *    *-----------------------------------------------------------*ELGAOL  
01933 *    *  PERFORMED BY 0200-PRINT-THE-SUB-HEADER,                  *ELGAOL  
01934 *    *      0200-DISPLAY-NO-ACCUMS-MESSAGE,                      *ELGAOL  
01935 *    *      0905-TEXT-COMPRESSION-PROCESS,                       *ELGAOL  
01936 *    *      1240-PRINT-THE-HEADER.                               *ELGAOL  
01937 *    *-----------------------------------------------------------*ELGAOL  
01938      ADD  +1      TO  COF-NBR-DTL-LINES.                          ELGAOL  
01939      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGAOL  
01940      PERFORM 1300-LINK-TO-OUTPUT-MODULE THRU 1300-END.            ELGAOL  
01941      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGAOL  
01942    0990-END.                                                      ELGAOL  
01943      EXIT.                                                        ELGAOL  
01944                                                                   ELGAOL  
01945                                                                   ELGAOL  
01946 /***********************************************************      ELGAOL  
01947 *                                                          *      ELGAOL  
01948 *        DELETE ACCUM OCCURENCE FILE                       *      ELGAOL  
01949 *                                                          *      ELGAOL  
01950 ************************************************************      ELGAOL  
01951  1000-DELETE-ACCUM-OCCURENCE-FI.                                  ELGAOL  
01952 *    *-----------------------------------------------------------*ELGAOL  
01953 *    *  PERFORMED BY 0250-DISPLAY-REGULAR-TEXT.                  *ELGAOL  
01954 *    *-----------------------------------------------------------*ELGAOL  
01955      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGAOL  
01956      SET  IOP-DEL           TO  TRUE.                             ELGAOL  
01957      SET  IOP-FCQ-NONE      TO  TRUE.                             ELGAOL  
01958      SET  IOP-KVQ-NONE      TO  TRUE.                             ELGAOL  
01959      CALL 'ELUIOPGM' USING DFHEIBLK                               ELGAOL  
01960                            DFHCOMMAREA                            ELGAOL  
01961                            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS ELGAOL  
01962                            END-CALL.                              ELGAOL  
01963    1000-END.                                                      ELGAOL  
01964      EXIT.                                                        ELGAOL  
01965                                                                   ELGAOL  
01966                                                                   ELGAOL  
01967 /***********************************************************      ELGAOL  
01968 *                                                          *      ELGAOL  
01969 *        END THE DISPLAY                                   *      ELGAOL  
01970 *                                                          *      ELGAOL  
01971 ************************************************************      ELGAOL  
01972  1200-END-THE-DISPLAY.                                            ELGAOL  
01973 *    *-----------------------------------------------------------*ELGAOL  
01974 *    *  PERFORMED BY 0180-PROCESS.                               *ELGAOL  
01975 *    *-----------------------------------------------------------*ELGAOL  
01976      IF COF-NBR-DTL-LINES > 0                                     ELGAOL  
01977         PERFORM 1300-LINK-TO-OUTPUT-MODULE THRU 1300-END.         ELGAOL  
01978      SET COF-END  TO  TRUE.                                       ELGAOL  
01979      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELGAOL  
01980      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGAOL  
01981      PERFORM 1300-LINK-TO-OUTPUT-MODULE THRU 1300-END.            ELGAOL  
01982    1200-END.                                                      ELGAOL  
01983      EXIT.                                                        ELGAOL  
01984                                                                   ELGAOL  
01985                                                                   ELGAOL  
01986 ************************************************************      ELGAOL  
01987 *                                                          *      ELGAOL  
01988 *        LINK TO OUTPUT MODULE                             *      ELGAOL  
01989 *                                                          *      ELGAOL  
01990 ************************************************************      ELGAOL  
01991  1300-LINK-TO-OUTPUT-MODULE.                                      ELGAOL  
01992 *    *-----------------------------------------------------------*ELGAOL  
01993 *    *  PERFORMED BY 0210-DISPLAY-NOT-APPLICABLE-LO,             *ELGAOL  
01994 *    *      0310-CREATE-MULTI-PCENT-PHRA,                        *ELGAOL  
01995 *    *      0320-CREATE-PCENT-VALUE-PHRA,                        *ELGAOL  
01996 *    *      1200-END-THE-DISPLAY,                                *ELGAOL  
01997 *    *      0960-DISPLAY-O-P-X-END-SENTNCE                       *ELGAOL  
01998 *    *      0990-SETUP-FOR-OUTPUT-LINK.                          *ELGAOL  
01999 *    *-----------------------------------------------------------*ELGAOL  
02000      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELGAOL  
02001                     COMMAREA (DFHCOMMAREA)                        ELGAOL  
02002                     LENGTH (LENGTH OF DFHCOMMAREA)                ELGAOL  
02003                     END-EXEC.                                     ELGAOL  
02004    1300-END.                                                      ELGAOL  
02005      EXIT.                                                        ELGAOL  
