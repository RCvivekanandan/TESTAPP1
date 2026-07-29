00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGACL  
00003  PROGRAM-ID.         ELGACL.                                         LV002
00004                                                                   ELGACL  
00005  AUTHOR.             LUCY TORRES.                                 ELGACL  
00006                                                                   ELGACL  
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGACL  
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGACL  
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGACL  
00010                      233 N. MICHIGAN AVE                          ELGACL  
00011                      CHICAGO, ILLINOIS 60601                      ELGACL  
00012                                                                   ELGACL  
00013                                                                   ELGACL  
00014  DATE-WRITTEN.       08-JUL-1987.                                 ELGACL  
00015                                                                   ELGACL  
00016  DATE-COMPILED.                                                   ELGACL  
00017                                                                   ELGACL  
00018  SECURITY.           COPYRIGHT 1986,                              ELGACL  
00019                      HEALTH CARE SERVICE CORPORATION              ELGACL  
00020      SKIP3                                                        ELGACL  
00021                                                                   ELGACL  
00022  ENVIRONMENT DIVISION.                                            ELGACL  
00023                                                                   ELGACL  
00024  CONFIGURATION SECTION.                                           ELGACL  
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELGACL  
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELGACL  
00027      EJECT                                                        ELGACL  
00028 ******************************************************************ELGACL  
00029 *                                                                *ELGACL  
00030 *  ELGACL   - ELS:  GENERATES THE OUTPUT FOR COINSURANCE AT THE  *ELGACL  
00031 *                   TOPIC, BENEFIT PROVISION, AND COST           *ELGACL  
00032 *                   CONTAINMENT LEVELS.                          *ELGACL  
00033 *                                                                *ELGACL  
00034 ******************************************************************ELGACL  
00035 *                                                                *ELGACL  
00036 *                      MAINTENANCE HISTORY                       *ELGACL  
00037 *                                                                *ELGACL  
00038 *  MOD     DATE     BY  DRPT                ACTION               *ELGACL  
00039 * ----- ----------- --- ----- ---------------------------------- *ELGACL  
00040 * 01.00 08-JUL-1987 LET       CREATED                            *ELGACL  
00041 *                                                                *ELGACL  
00042 * 01.01 27-AUG-1987 LET       SEPERATED THE COST CONTAINMENT     *ELGACL  
00043 *                             LEVEL PROCESSING FROM THE OTHER    *ELGACL  
00044 *                             LEVELS OF PROCESSING.              *ELGACL  
00045 *                                                                *ELGACL  
00046 * 01.02 30-SEP-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELGACL  
00047 *                             VERSION OF COPYBOOK ELSACCUM.      *ELGACL  
00048 *                                                                *ELGACL  
00049 * 01.03 01-OCT-1987 REB       ADDED LOGIC TO HANDLE PROCESSING   *ELGACL  
00050 *                             FOR GROUP/CONTRACT ACCUMS.         *ELGACL  
00051 *                                                                *ELGACL  
00052 * 01.04 09-OCT-1987 LET       DELETED LOGIC FOR GROUP/CONTRACT   *ELGACL  
00053 *                             ACCUMS.                            *ELGACL  
00054 *                                                                *ELGACL  
00055 * 01.05 09-NOV-1987 REB       ADDED LOGIC TO HANDLE MULTIPLE     *ELGACL  
00056 *                             INTERNALS FOR ASCEND/DESCEND IF    *ELGACL  
00057 *                             THEY EXIST.                        *ELGACL  
00058 *                                                                *ELGACL  
00059 * 01.06 10-NOV-1987 REB       CORRECTED CONDITIONALS IN THE      *ELGACL  
00060 *                             APPLICABILITY PHRASE.              *ELGACL  
00061 *                                                                *ELGACL  
00062 * 01.07 13-NOV-1987 REB       CHANGED CODE TO ASSOCIATE THE      *ELGACL  
00063 *                             CORRECT PERCENT LEVEL WITH ACTUAL  *ELGACL  
00064 *                             INTERNAL TABULAR.                  *ELGACL  
00065 *                                                                *ELGACL  
00066 * 01.08 19-NOV-1987 REB       USED PERCENT LEVEL PASSED BY SETUP *ELGACL  
00067 *                             FOR INTERNAL TABULARS WITH ASC/DESC*ELGACL  
00068 *                                                                *ELGACL  
00069 * 01.08 03-JAN-1991 AKK       STORAGE MANAGEMENT ENHANCEMENTS    *ELGACL  
00070 *                                                                *ELGACL  
00071 * 01.09 08-FEB-1991 AKK       ADD SUPPORT FOR IPGP AND IDGD INT- *ELGACL  
00072 *                             TERNAL TABULARS.                   *ELGACL  
00073 *                                                                *ELGACL  
00074 * 01.10 08-NOV-1991 JPB       ENHANCED DISPLAY OF ACCUMULATION   *ELGACL  
00075 *                             LEVELS FOR INTERNALS.              *ELGACL  
00076 *                                                                *ELGACL  
00077 * 01.11 08-NOV-1993 JPB       ADJUSTED 88-LEVEL VALUE FOR \
00078 *                             QUALIFIER SO THE WORD \
00079 *                             WOULD BE DISPLAYED INSTEAD OF      *ELGACL  
00080 *                             999,999,999.  ALSO COINS-INTERVAL- *ELGACL  
00081 *                             TIME-QUAL WAS CHANGED TO COINS-    *ELGACL  
00082 *                             INTERVAL-TIME-FCTR.                *ELGACL  
00083 *                                                                *ELGACL  
00084 * 01.12 21-AUG-2000 AKK       ADDED SUPPORT FOR #IPGS            *ELGACL  
00085 *                                                                *ELGACL  
00086 * 01.13 29-NOV-2000 AKK       IN 0740-CHECK-FOR-INTERNALS ADDED  *ELGACL  
00087 *                             CHECK FOR LOW-VALUES TO TRY AND    *ELGACL  
00088 *                             STOP S0C7S DUE TO SPACES IN ONE    *ELGACL  
00089 *                             OCCURANCE OF ACCUM TABLE.          *ELGACL  
00090 *       12-AUG-2003 AKK GEN IN QE TO TEST ORDER OF COMPILE       *ELGACL  
ED0624*                                                                *        
ED0624* BBDA-58217 06/14/24  ED     RECOMPILE FOR PEAQ COPYBOOK        *        
ED0624*                             EXPANSION:                         *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00091 ******************************************************************ELGACL  
00092                                                                   ELGACL  
00093  DATA DIVISION.                                                   ELGACL  
00094  WORKING-STORAGE SECTION.                                         ELGACL  
00095  77  PAN-VALET PICTURE X(24) VALUE '004ELGACLTS  03/08/91'.       ELGACL  
00096  77  PAN-DSN   PICTURE  X(44) VALUE                               ELGACL  
00097      'HCMSGEN.TEST.PANLIB                         '.              ELGACL  
00098 /                                                                 ELGACL  
00099  01  HEADERS.                                                     ELGACL  
00100      05  TOPIC-HEADER .                                           ELGACL  
00101          10  FILLER                  PIC  X(34) VALUE SPACES.     ELGACL  
00102          10  FILLER                  PIC  X(11) VALUE             ELGACL  
00103                  'COINSURANCE'.                                   ELGACL  
00104          10  FILLER                  PIC  X(34) VALUE SPACES.     ELGACL  
00105      05  BENEFIT-PROVISION-HEADER.                                ELGACL  
00106          10  FILLER                  PIC  X(29) VALUE             ELGACL  
00107                  'COINSURANCE AT BENEFIT LEVEL:'.                 ELGACL  
00108          10  FILLER                  PIC  X(50) VALUE SPACES.     ELGACL  
00109                                                                   ELGACL  
00110  01  MESSAGES.                                                    ELGACL  
00111      05  COINSURANCE-OTHR-SRCE-MSG   PIC  X(34) VALUE             ELGACL  
00112              'AN AMOUNT FOUND IN ANOTHER SOURCE.'.                ELGACL  
00113      05  COINSURANCE-END-MSG-1A      PIC  X(79) VALUE             ELGACL  
00114              'ADDITIONAL COINSURANCE MAY APPLY TO INDIVIDUAL BENEFELGACL  
00115 -            'ITS.  SEE SPECIFIC TOPICS  '.                       ELGACL  
00116      05  COINSURANCE-END-MSG-1B      PIC  X(27) VALUE             ELGACL  
00117              'FOR ADDITIONAL COINSURANCE.'.                       ELGACL  
00118      05  NO-ACCUMS-MSG               PIC  X(47) VALUE             ELGACL  
00119              'NO GROUP OR CONTRACT LEVEL COINSURANCE APPLIES.'.   ELGACL  
00120      05  NO-INST-LOB-MSG             PIC  X(61) VALUE             ELGACL  
00121              'NO INSTITUTIONAL GROUP OR CONTRACT LEVEL COINSURANCEELGACL  
00122 -            ' APPLIES.'.                                         ELGACL  
00123      05  NO-PROF-LOB-MSG             PIC  X(61) VALUE             ELGACL  
00124              'NO PROFESSIONAL GROUP OR CONTRACT LEVEL COINSURANCE ELGACL  
00125 -            ' APPLIES.'.                                         ELGACL  
00126                                                                   ELGACL  
00127  01  PROGRAM-CONSTANTS.                                           ELGACL  
00128      05  PC-ACL                      PIC  X(06) VALUE             ELGACL  
00129              '#ACL  '.                                            ELGACL  
00130      05  PC-AND                      PIC  X(04) VALUE             ELGACL  
00131              ' AND'.                                              ELGACL  
00132      05  PC-AND-APPLIES-TO           PIC  X(16) VALUE             ELGACL  
00133              ' AND APPLIES TO '.                                  ELGACL  
00134      05  PC-AND-IS                   PIC  X(08) VALUE             ELGACL  
00135              ' AND IS '.                                          ELGACL  
00136      05  PC-APPLIES                  PIC  X(09) VALUE             ELGACL  
00137              ' APPLIES '.                                         ELGACL  
00138      05  PC-CHANGES-TO               PIC  X(16) VALUE             ELGACL  
00139              'THEN CHANGES TO '.                                  ELGACL  
00140      05  PC-COINSURANCE-APPLIES      PIC  X(24) VALUE             ELGACL  
00141              ' COINSURANCE APPLIES TO '.                          ELGACL  
00142      05  PC-COMMA                    PIC  X(01) VALUE ','.        ELGACL  
00143      05  PC-CONSIDERATIONS           PIC  X(29) VALUE             ELGACL  
00144              ' CONSIDERATIONS LISTED BELOW '.                     ELGACL  
00145      05  PC-DOLLARS                  PIC  X(07) VALUE             ELGACL  
00146              'DOLLARS'.                                           ELGACL  
00147      05  PC-DOTS                     PIC  X(59) VALUE             ELGACL  
00148              '....................................................ELGACL  
00149 -            '.......'.                                           ELGACL  
00150      05  PC-FOR                      PIC  X(05) VALUE             ELGACL  
00151              ' FOR '.                                             ELGACL  
00152      05  PC-IBGR                     PIC  X(06) VALUE '#IBGR '.   ELGACL  
00153      05  PC-IBGR-NAME                PIC  X(18) VALUE             ELGACL  
00154              ' BENEFIT PROVISION'.                                ELGACL  
00155      05  PC-IPGN                     PIC  X(06) VALUE '#IPGN '.   ELGACL  
00156      05  PC-IPGN-NAME                PIC  X(18) VALUE             ELGACL  
00157              '   PROVIDER NUMBER'.                                ELGACL  
00158      05  PC-IPGT                     PIC  X(06) VALUE '#IPGT '.   ELGACL  
00159      05  PC-IPGT-NAME                PIC  X(18) VALUE             ELGACL  
00160              '     PROVIDER TYPE'.                                ELGACL  
00161      05  PC-IPGP                     PIC  X(06) VALUE '#IPGP '.   ELGACL  
00162      05  PC-IPGP-NAME                PIC  X(18) VALUE             ELGACL  
00163              '         PROCEDURE'.                                ELGACL  
00164      05  PC-IDGD                     PIC  X(06) VALUE '#IDGD '.   ELGACL  
00165      05  PC-IDGD-NAME                PIC  X(18) VALUE             ELGACL  
00166              '         DIAGNOSIS'.                                ELGACL  
00167      05  PC-IPGS                     PIC  X(06) VALUE '#IPGS '.   ELGACL  
00168      05  PC-IPGS-NAME                PIC  X(18) VALUE             ELGACL  
00169              'PROVIDER SPECIALTY'.                                ELGACL  
00170      05  PC-MULTI-PCENT-START        PIC  X(19) VALUE             ELGACL  
00171              'THE COINSURANCE IS:'.                               ELGACL  
00172      05  PC-NOTE                     PIC  X(08) VALUE             ELGACL  
00173              ' (NOTE: '.                                          ELGACL  
00174      05  PC-PER                      PIC  X(05) VALUE             ELGACL  
00175              ' PER '.                                             ELGACL  
00176      05  PC-PROVIDED                 PIC  X(10) VALUE             ELGACL  
00177              ' PROVIDED '.                                        ELGACL  
00178      05  PC-SERVICES                 PIC  X(10) VALUE             ELGACL  
00179              ' SERVICES '.                                        ELGACL  
00180      05  PC-SPACES                   PIC  X(16) VALUE             ELGACL  
00181              '                '.                                  ELGACL  
00182      05  PC-SUBJECT-TO               PIC  X(13) VALUE             ELGACL  
00183              ', SUBJECT TO '.                                     ELGACL  
00184      05  PC-THIS                     PIC  X(05) VALUE             ELGACL  
00185              'THIS '.                                             ELGACL  
00186      05  PC-TO                       PIC  X(04) VALUE             ELGACL  
00187              ' TO '.                                              ELGACL  
00188      05  PC-UNLIMITED                PIC  X(10) VALUE             ELGACL  
00189              'UNLIMITED '.                                        ELGACL  
00190                                                                   ELGACL  
00191                                                                   ELGACL  
00192  01  WS-DEFINITION-LINE.                                          ELGACL  
00193      05  FILLER                   PIC  X(30) VALUE                ELGACL  
00194              'THE COINSURANCE IS CALCULATED '.                    ELGACL  
00195                                                                   ELGACL  
00196  01  WS-BEN-PER-PHRASE.                                           ELGACL  
00197      05  FILLER                   PIC  X(28) VALUE                ELGACL  
00198              'THE COINSURANCE APPLIES PER '.                      ELGACL  
00199                                                                   ELGACL  
00200  01  WS-BP-TIME-FCTR-QUAL-PHRASE.                                 ELGACL  
00201      05  FILLER                   PIC  X(11) VALUE                ELGACL  
00202              ' PERIOD OF '.                                       ELGACL  
00203      05  WS-BP-TIME-FCTR          PIC  ZZ9.                       ELGACL  
00204      05  WS-BP-TIME-FCTR-X REDEFINES                              ELGACL  
00205            WS-BP-TIME-FCTR        PIC  X(03).                     ELGACL  
00206      05  FILLER                   PIC  X(01) VALUE SPACE.         ELGACL  
00207      05  WS-BP-TIME-QUAL          PIC  X(65).                     ELGACL  
00208                                                                   ELGACL  
00209  01  WS-INTERVAL-TIME-FCTR-QUAL-PHR.                              ELGACL  
00210      05  FILLER                   PIC  X(14) VALUE                ELGACL  
00211              ' SEPARATED BY '.                                    ELGACL  
00212      05  WS-INTERVAL-TIME-FCTR    PIC  ZZ9.                       ELGACL  
00213      05  WS-INTERVAL-TIME-FCTR-X REDEFINES                        ELGACL  
00214            WS-INTERVAL-TIME-FCTR  PIC  X(03).                     ELGACL  
00215      05  FILLER                   PIC  X(01) VALUE SPACE.         ELGACL  
00216      05  WS-INTERVAL-TIME-QUAL    PIC  X(62).                     ELGACL  
00217                                                                   ELGACL  
00218  01  WS-INTERVAL-OVERRIDE-PHRASE-1.                               ELGACL  
00219      05  FILLER                   PIC  X(33) VALUE                ELGACL  
00220              'THE INTERVAL MAY BE OVERRULED IF '.                 ELGACL  
00221      05  WS-INTERVAL-OVERRIDE-VALUE-1        PIC ZZZZ9.           ELGACL  
00222      05  FILLER                   PIC  X(76) VALUE                ELGACL  
00223          ' MONTHS HAVE ELAPSED FROM THE ADMISSION DATE OF THE FIRSELGACL  
00224 -            'T COVERED ADMISSION.'.                              ELGACL  
00225                                                                   ELGACL  
00226  01  WS-INTERVAL-OVERRIDE-PHRASE-2.                               ELGACL  
00227      05  FILLER                   PIC  X(73) VALUE                ELGACL  
00228              'IF THE MEMBER IS MEDICARE ELIGIBLE, THE BENEFIT PERIELGACL  
00229 -            'ODS ARE SEPARATED BY '.                             ELGACL  
00230      05  WS-INTERVAL-OVERRIDE-VALUE-2        PIC ZZZZ9.           ELGACL  
00231      05  FILLER                   PIC  X(06) VALUE ' DAYS.'.      ELGACL  
00232                                                                   ELGACL  
00233  01  WS-INTERVAL-OVERRIDE-PHRASE-3.                               ELGACL  
00234      05  FILLER                   PIC  X(38) VALUE                ELGACL  
00235              'THE INTERVAL CAN BE OVERRULED SO THAT '.            ELGACL  
00236      05  WS-INTERVAL-OVERRIDE-VALUE-3        PIC ZZZZ9.           ELGACL  
00237      05  FILLER                   PIC  X(53) VALUE                ELGACL  
00238          ' DAYS/VISITS ARE PAID AT THE INDICATED PERCENT LEVEL.'. ELGACL  
00239                                                                   ELGACL  
00240  01  WS-FROM-AGE-QUAL-PHRASE.                                     ELGACL  
00241      05  FILLER                   PIC  X(10) VALUE                ELGACL  
00242              ' FROM AGE '.                                        ELGACL  
00243      05  WS-FROM-AGE              PIC  ZZZZZZZZZ9.                ELGACL  
00244      05  WS-FROM-AGE-ALPHA REDEFINES                              ELGACL  
00245            WS-FROM-AGE            PIC  X(10).                     ELGACL  
00246      05  FILLER                   PIC  X(01) VALUE SPACES.        ELGACL  
00247      05  WS-FROM-AGE-QUAL         PIC  X(12).                     ELGACL  
00248                                                                   ELGACL  
00249  01  WS-TO-AGE-QUAL-PHRASE.                                       ELGACL  
00250      05  FILLER                   PIC  X(10) VALUE                ELGACL  
00251              ' TO AGE '.                                          ELGACL  
00252      05  WS-TO-AGE                PIC  ZZZZZZZZZ9.                ELGACL  
00253      05  WS-TO-AGE-ALPHA REDEFINES                                ELGACL  
00254            WS-TO-AGE              PIC  X(10).                     ELGACL  
00255      05  FILLER                   PIC  X(01) VALUE SPACES.        ELGACL  
00256      05  WS-TO-AGE-QUAL           PIC  X(12).                     ELGACL  
00257                                                                   ELGACL  
00258  01  WS-VAL-LIMIT-AMOUNT.                                         ELGACL  
00259      05  WS-VAL-LIM-DOLLARS       PIC  $$,$$$,$$9.99.             ELGACL  
00260          88  WS-MAX-LIMIT-DOLLARS            VALUE                ELGACL  
00261              '$9,999,999.99' '$9,999,999.00'.                     ELGACL  
00262      05  FILLER                   PIC  X(34).                     ELGACL  
00263                                                                   ELGACL  
00264  01  WS-VAL-LIMIT-OTHER-AMT REDEFINES WS-VAL-LIMIT-AMOUNT.        ELGACL  
00265      05  WS-VAL-LIM-OTHER         PIC  ZZ,ZZZ,Z99.                ELGACL  
00266          88  WS-MAX-LIMIT-OTHER              VALUE                ELGACL  
00267              '99,999,999' '99,999,900'.                           ELGACL  
00268      05  WS-VAL-QUAL-OTHER        PIC  X(37).                     ELGACL  
00269                                                                   ELGACL  
00270  01  WS-VAL-LIMIT-UNLIMITED REDEFINES WS-VAL-LIMIT-AMOUNT.        ELGACL  
00271      05  WS-VAL-QUAL-UNLIMITED    PIC  X(47).                     ELGACL  
00272                                                                   ELGACL  
00273  01  WS-SINGLE-PERCENT-PHRASE.                                    ELGACL  
00274      05  FILLER                   PIC  X(19) VALUE                ELGACL  
00275              'THE COINSURANCE IS '.                               ELGACL  
00276      05  WS-SINGLE-PERCENT-LEVEL  PIC ZZ9    VALUE ZERO.          ELGACL  
00277      05  FILLER                   PIC  X(8)  VALUE                ELGACL  
00278              '% UP TO '.                                          ELGACL  
00279      05  WS-SINGLE-VAL-LMT-PHR    PIC  X(47).                     ELGACL  
00280      05  FILLER                   PIC  X(01) VALUE SPACES.        ELGACL  
00281                                                                   ELGACL  
00282  01  WS-MULTI-VALUE-PHRASE.                                       ELGACL  
00283      05  WS-MULTI-PERCENT-LEVEL   PIC  ZZ9   VALUE ZERO.          ELGACL  
00284      05  FILLER                   PIC  X(08) VALUE                ELGACL  
00285          '% UP TO '.                                              ELGACL  
00286      05  WS-MULTI-VAL-LMT-PHR     PIC  X(47) VALUE SPACES.        ELGACL  
00287                                                                   ELGACL  
00288  01  WS-MULTI-VALUE-LINE.                                         ELGACL  
00289      05  WS-MVL-CHANGES-PHRASE    PIC  X(16) VALUE SPACES.        ELGACL  
00290      05  WS-MVL-MASK              PIC  X(58) VALUE SPACES.        ELGACL  
00291      05  FILLER                   PIC  X(01) VALUE SPACES.        ELGACL  
00292      05  WS-MVL-LEVEL-TAG         PIC  X(04) VALUE SPACES.        ELGACL  
00293                                                                   ELGACL  
00294  01  WS-INTERNAL-TAB-SWITCHES.                                    ELGACL  
00295      05                          PIC  X.                          ELGACL  
00296          88  WS-HAS-AN-INTERNAL             VALUE 'Y'.            ELGACL  
00297          88  WS-HAS-NO-INTERNAL             VALUE 'N'.            ELGACL  
00298                                                                   ELGACL  
00299      05                          PIC  X.                          ELGACL  
00300          88  WS-HAS-IBGR                    VALUE 'Y'.            ELGACL  
00301          88  WS-HAS-NO-IBGR                 VALUE 'N'.            ELGACL  
00302                                                                   ELGACL  
00303      05                          PIC  X.                          ELGACL  
00304          88  WS-HAS-IDGD                    VALUE 'Y'.            ELGACL  
00305          88  WS-HAS-NO-IDGD                 VALUE 'N'.            ELGACL  
00306                                                                   ELGACL  
00307      05                          PIC  X.                          ELGACL  
00308          88  WS-HAS-IPGN                    VALUE 'Y'.            ELGACL  
00309          88  WS-HAS-NO-IPGN                 VALUE 'N'.            ELGACL  
00310                                                                   ELGACL  
00311      05                          PIC  X.                          ELGACL  
00312          88  WS-HAS-IPGP                    VALUE 'Y'.            ELGACL  
00313          88  WS-HAS-NO-IPGP                 VALUE 'N'.            ELGACL  
00314                                                                   ELGACL  
00315      05                          PIC  X.                          ELGACL  
00316          88  WS-HAS-IPGT                    VALUE 'Y'.            ELGACL  
00317          88  WS-HAS-NO-IPGT                 VALUE 'N'.            ELGACL  
00318                                                                   ELGACL  
00319      05                          PIC  X.                          ELGACL  
00320          88  WS-HAS-IPGS                    VALUE 'Y'.            ELGACL  
00321          88  WS-HAS-NO-IPGS                 VALUE 'N'.            ELGACL  
00322                                                                   ELGACL  
00323  01  WS-INT-TAB-LST.                                              ELGACL  
00324      02 WS-TAB-SUB                PIC S9(04) COMP.                ELGACL  
00325      02 WS-INT-TAB                OCCURS 6 TIMES.                 ELGACL  
00326         03                        PIC  X(18).                     ELGACL  
00327            88 WS-INT-TAB-IBGR VALUE ' BENEFIT PROVISION'.         ELGACL  
00328            88 WS-INT-TAB-IDGD VALUE '         DIAGNOSIS'.         ELGACL  
00329            88 WS-INT-TAB-IPGN VALUE '   PROVIDER NUMBER'.         ELGACL  
00330            88 WS-INT-TAB-IPGP VALUE '         PROCEDURE'.         ELGACL  
00331            88 WS-INT-TAB-IPGT VALUE '     PROVIDER TYPE'.         ELGACL  
00332            88 WS-INT-TAB-IPGS VALUE 'PROVIDER SPECIALTY'.         ELGACL  
00333         03                        PIC  X(05).                     ELGACL  
00334            88 WS-INT-TAB-AND      VALUE ' AND '.                  ELGACL  
00335            88 WS-INT-TAB-COMMA    VALUE ',    '.                  ELGACL  
00336            88 WS-INT-TAB-END      VALUE SPACES.                   ELGACL  
00337                                                                   ELGACL  
00338  01  WS-INT-TAB-TXT               REDEFINES WS-INT-TAB-LST.       ELGACL  
00339      02                           PIC S9(04) COMP.                ELGACL  
00340      02 WS-INT-TAB-TXT-1          PICTURE  X(69).                 ELGACL  
00341      02 WS-INT-TAB-TXT-2          PICTURE  X(46).                 ELGACL  
00342                                                                   ELGACL  
00343  01  WS-TEXT-HOLD-AREA.                                           ELGACL  
00344      02  WS-TEXT-HOLD-COUNT      PIC S9(4) COMP.                  ELGACL  
00345      02  WS-TEXT-HOLD-TEXT       PIC X(1580).                     ELGACL  
00346                                                                   ELGACL  
00347  01  COUNTERS.                                                    ELGACL  
00348      02  WS-VLT-SUB              PIC S9(4) COMP.                  ELGACL  
00349                                                                   ELGACL  
00350      COPY ELSVLTGC.                                               ELGACL  
00351                                                                   ELGACL  
00352  LINKAGE SECTION.                                                 ELGACL  
00353  01  DFHCOMMAREA.                                                 ELGACL  
00354      COPY ELSCOMMC.                                               ELGACL  
00355                                                                   ELGACL  
00356      COPY ELSCIA2C.                                               ELGACL  
00357                                                                   ELGACL  
00358      COPY ELSIOPMC.                                               ELGACL  
00359                                                                   ELGACL  
00360      COPY ELSCMIFC.                                               ELGACL  
00361                                                                   ELGACL  
00362      COPY ELSCMDSC.                                               ELGACL  
00363                                                                   ELGACL  
00364      COPY ELSOUTPC.                                               ELGACL  
00365                                                                   ELGACL  
00366      COPY ELSSRTPC.                                               ELGACL  
00367                                                                   ELGACL  
00368      COPY ELSSSCBC.                                               ELGACL  
00369                                                                   ELGACL  
00370      COPY ELSTCWAC.                                               ELGACL  
00371                                                                   ELGACL  
00372      COPY ELSACUMC.                                               ELGACL  
00373                                                                   ELGACL  
00374 /***********************************************************      ELGACL  
00375 *                                                          *      ELGACL  
00376 *                    PROCEDURE DIVISION                    *      ELGACL  
00377 *                                                          *      ELGACL  
00378 ************************************************************      ELGACL  
00379  PROCEDURE DIVISION.                                              ELGACL  
00380                                                                   ELGACL  
00381                                                                   ELGACL  
00382 ************************************************************      ELGACL  
00383 *                                                          *      ELGACL  
00384 *        PROCESS ELGACL                                    *      ELGACL  
00385 *                                                          *      ELGACL  
00386 ************************************************************      ELGACL  
00387      PERFORM 0010-INITIALIZATION THRU 0010-END.                   ELGACL  
00388      PERFORM 0170-PROCESS THRU 0170-END.                          ELGACL  
00389      GOBACK.                                                      ELGACL  
00390                                                                   ELGACL  
00391 ************************************************************      ELGACL  
00392 *                                                          *      ELGACL  
00393 *        INITIALIZATION                                    *      ELGACL  
00394 *                                                          *      ELGACL  
00395 ************************************************************      ELGACL  
00396  0010-INITIALIZATION.                                             ELGACL  
00397      PERFORM 0020-EST-ADR-CNTL-BLKS THRU 0020-END.                ELGACL  
00398      PERFORM 0090-EST-ADR-WORK-AREAS THRU 0090-END.               ELGACL  
00399      IF COF-NBR-DTL-LINES > 0                                     ELGACL  
00400      THEN                                                         ELGACL  
00401         CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.      ELGACL  
00402                                                                   ELGACL  
00403      INITIALIZE TCAR-FROM-AREA                                    ELGACL  
00404                 TCAR-FROM-LENGTH                                  ELGACL  
00405                 TCAR-FROM-SUB.                                    ELGACL  
00406                                                                   ELGACL  
00407    0010-END.                                                      ELGACL  
00408      EXIT.                                                        ELGACL  
00409                                                                   ELGACL  
00410                                                                   ELGACL  
00411 ************************************************************      ELGACL  
00412 *                                                          *      ELGACL  
00413 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELGACL  
00414 *                                                          *      ELGACL  
00415 ************************************************************      ELGACL  
00416  0020-EST-ADR-CNTL-BLKS.                                          ELGACL  
00417 *    *-----------------------------------------------------------*ELGACL  
00418 *    *  PERFORMED BY 0010-INITIALIZATION.                        *ELGACL  
00419 *    *-----------------------------------------------------------*ELGACL  
00420 *                                                          *      ELGACL  
00421 *        CHECK FOR VALID COMMAREA                          *      ELGACL  
00422 *                                                          *      ELGACL  
00423      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGACL  
00424         EXEC CICS ABEND                                           ELGACL  
00425                ABCODE('EL01')                                     ELGACL  
00426         END-EXEC.                                                 ELGACL  
00427                                                                   ELGACL  
00428 *                                                          *      ELGACL  
00429 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELGACL  
00430 *                                                          *      ELGACL  
00431      IF ECA-CIA-PTR = NULL                                        ELGACL  
00432         EXEC CICS ABEND                                           ELGACL  
00433                ABCODE('EL02')                                     ELGACL  
00434         END-EXEC                                                  ELGACL  
00435      ELSE                                                         ELGACL  
00436         CALL 'ELUINISM' USING DFHCOMMAREA                         ELGACL  
00437              ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.            ELGACL  
00438 *                                                          *      ELGACL  
00439 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELGACL  
00440 *                                                          *      ELGACL  
00441      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGACL  
00442      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACL  
00443           ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                 ELGACL  
00444      IF CIA-RC-PTR-NULL                                           ELGACL  
00445          PERFORM 0150-SIGNAL-UNALLOC-AREA-ERROR THRU 0150-END.    ELGACL  
00446    0020-END.                                                      ELGACL  
00447      EXIT.                                                        ELGACL  
00448                                                                   ELGACL  
00449 /***********************************************************      ELGACL  
00450 *                                                          *      ELGACL  
00451 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELGACL  
00452 *                                                          *      ELGACL  
00453 ************************************************************      ELGACL  
00454  0090-EST-ADR-WORK-AREAS.                                         ELGACL  
00455 *    *-----------------------------------------------------------*ELGACL  
00456 *    *  PERFORMED BY 0010-INITIALIZATION.                        *ELGACL  
00457 *    *-----------------------------------------------------------*ELGACL  
00458      PERFORM 0100-EST-ADR-TEMP-FILE THRU 0100-END.                ELGACL  
00459      PERFORM 0110-EST-ADR-CODES-MANUAL-INT THRU 0110-END.         ELGACL  
00460      PERFORM 0120-EST-ADR-OUTP-INT THRU 0120-END.                 ELGACL  
00461      PERFORM 0130-EST-ADR-SUBROUTINE-PARAMS THRU 0130-END.        ELGACL  
00462      PERFORM 0140-EST-ADR-TCAR-WORK THRU 0140-END.                ELGACL  
00463    0090-END.                                                      ELGACL  
00464      EXIT.                                                        ELGACL  
00465                                                                   ELGACL  
00466                                                                   ELGACL  
00467 ************************************************************      ELGACL  
00468 *                                                          *      ELGACL  
00469 *        ESTABLISH ADDRESSABILITY OF TMPORARY FILE         *      ELGACL  
00470 *                                                          *      ELGACL  
00471 ************************************************************      ELGACL  
00472  0100-EST-ADR-TEMP-FILE.                                          ELGACL  
00473 *    *-----------------------------------------------------------*ELGACL  
00474 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREAS.                    *ELGACL  
00475 *    *-----------------------------------------------------------*ELGACL  
00476      SET CIA-ELSWKFL1-DDN  TO  TRUE.                              ELGACL  
00477      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACL  
00478             ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.               ELGACL  
00479      IF CIA-RC-PTR-NULL                                           ELGACL  
00480          PERFORM 0150-SIGNAL-UNALLOC-AREA-ERROR THRU 0150-END.    ELGACL  
00481    0100-END.                                                      ELGACL  
00482      EXIT.                                                        ELGACL  
00483      EJECT                                                        ELGACL  
00484                                                                   ELGACL  
00485                                                                   ELGACL  
00486 ************************************************************      ELGACL  
00487 *                                                          *      ELGACL  
00488 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE*      ELGACL  
00489 *                                                          *      ELGACL  
00490 ************************************************************      ELGACL  
00491  0110-EST-ADR-CODES-MANUAL-INT.                                   ELGACL  
00492 *    *-----------------------------------------------------------*ELGACL  
00493 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREAS.                    *ELGACL  
00494 *    *-----------------------------------------------------------*ELGACL  
00495      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGACL  
00496      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACL  
00497              ADDRESS OF CMF-CODES-MANUAL-INTERFACE.               ELGACL  
00498      IF CIA-RC-PTR-NULL                                           ELGACL  
00499          PERFORM 0150-SIGNAL-UNALLOC-AREA-ERROR THRU 0150-END.    ELGACL  
00500    0110-END.                                                      ELGACL  
00501      EXIT.                                                        ELGACL  
00502      EJECT                                                        ELGACL  
00503                                                                   ELGACL  
00504                                                                   ELGACL  
00505 ************************************************************      ELGACL  
00506 *                                                          *      ELGACL  
00507 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGACL  
00508 *                                                          *      ELGACL  
00509 ************************************************************      ELGACL  
00510  0120-EST-ADR-OUTP-INT.                                           ELGACL  
00511 *    *-----------------------------------------------------------*ELGACL  
00512 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREAS.                    *ELGACL  
00513 *    *-----------------------------------------------------------*ELGACL  
00514      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGACL  
00515      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACL  
00516              ADDRESS OF COF-OUTPUT-INTERFACE.                     ELGACL  
00517      IF CIA-RC-PTR-NULL                                           ELGACL  
00518          PERFORM 0150-SIGNAL-UNALLOC-AREA-ERROR THRU 0150-END.    ELGACL  
00519    0120-END.                                                      ELGACL  
00520      EXIT.                                                        ELGACL  
00521      EJECT                                                        ELGACL  
00522                                                                   ELGACL  
00523                                                                   ELGACL  
00524 ************************************************************      ELGACL  
00525 *                                                          *      ELGACL  
00526 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGACL  
00527 *                                                          *      ELGACL  
00528 ************************************************************      ELGACL  
00529  0130-EST-ADR-SUBROUTINE-PARAMS.                                  ELGACL  
00530 *    *-----------------------------------------------------------*ELGACL  
00531 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREAS.                    *ELGACL  
00532 *    *-----------------------------------------------------------*ELGACL  
00533      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGACL  
00534      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACL  
00535              ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                ELGACL  
00536      IF CIA-RC-PTR-NULL                                           ELGACL  
00537          PERFORM 0150-SIGNAL-UNALLOC-AREA-ERROR THRU 0150-END.    ELGACL  
00538    0130-END.                                                      ELGACL  
00539      EXIT.                                                        ELGACL  
00540      EJECT                                                        ELGACL  
00541                                                                   ELGACL  
00542                                                                   ELGACL  
00543 ************************************************************      ELGACL  
00544 *                                                          *      ELGACL  
00545 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELGACL  
00546 *                                                          *      ELGACL  
00547 ************************************************************      ELGACL  
00548  0140-EST-ADR-TCAR-WORK.                                          ELGACL  
00549 *    *-----------------------------------------------------------*ELGACL  
00550 *    *  PERFORMED BY 0090-EST-ADR-WORK-AREAS.                    *ELGACL  
00551 *    *-----------------------------------------------------------*ELGACL  
00552      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGACL  
00553      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACL  
00554              ADDRESS OF TCAR-COMPRESSION-WORK-AREA.               ELGACL  
00555      IF CIA-RC-PTR-NULL                                           ELGACL  
00556          PERFORM 0150-SIGNAL-UNALLOC-AREA-ERROR THRU 0150-END.    ELGACL  
00557    0140-END.                                                      ELGACL  
00558      EXIT.                                                        ELGACL  
00559                                                                   ELGACL  
00560                                                                   ELGACL  
00561 ************************************************************      ELGACL  
00562 *                                                          *      ELGACL  
00563 *        SIGNAL UNALLOC AREA ERROR                         *      ELGACL  
00564 *                                                          *      ELGACL  
00565 ************************************************************      ELGACL  
00566  0150-SIGNAL-UNALLOC-AREA-ERROR.                                  ELGACL  
00567 *    *-----------------------------------------------------------*ELGACL  
00568 *    *  PERFORMED BY 0080-EST-ADR,                               *ELGACL  
00569 *    *      0100-EST-ADR-TEMP-FILE,                              *ELGACL  
00570 *    *      0110-EST-ADR-CODES-MANUAL-INT,                       *ELGACL  
00571 *    *      0120-EST-ADR-OUTP-INT,                               *ELGACL  
00572 *    *      0130-EST-ADR-SUBROUTINE-PARAMS,                      *ELGACL  
00573 *    *      0140-EST-ADR-TCAR-WORK.                              *ELGACL  
00574 *    *-----------------------------------------------------------*ELGACL  
00575      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGACL  
00576      PERFORM 0160-SIGNAL-ABEND THRU 0160-END.                     ELGACL  
00577    0150-END.                                                      ELGACL  
00578      EXIT.                                                        ELGACL  
00579                                                                   ELGACL  
00580                                                                   ELGACL  
00581 ************************************************************      ELGACL  
00582 *                                                          *      ELGACL  
00583 *        SIGNAL ABEND                                      *      ELGACL  
00584 *                                                          *      ELGACL  
00585 ************************************************************      ELGACL  
00586  0160-SIGNAL-ABEND.                                               ELGACL  
00587 *    *-----------------------------------------------------------*ELGACL  
00588 *    *  PERFORMED BY 0150-SIGNAL-UNALLOC-AREA-ERROR.             *ELGACL  
00589 *    *-----------------------------------------------------------*ELGACL  
00590      EXEC CICS ABEND                                              ELGACL  
00591                ABCODE(CIA-ABCODE)                                 ELGACL  
00592         END-EXEC.                                                 ELGACL  
00593    0160-END.                                                      ELGACL  
00594      EXIT.                                                        ELGACL  
00595      EJECT                                                        ELGACL  
00596                                                                   ELGACL  
00597                                                                   ELGACL  
00598 ************************************************************      ELGACL  
00599 *                                                          *      ELGACL  
00600 *        PROCESS                                           *      ELGACL  
00601 *                                                          *      ELGACL  
00602 ************************************************************      ELGACL  
00603  0170-PROCESS.                                                    ELGACL  
00604      IF SRP-TOPIC-ACCUM                                           ELGACL  
00605          PERFORM 0960-OBTAIN-TOPIC-HEADER THRU 0960-END           ELGACL  
00606      ELSE IF SRP-BEN-PROV-ACCUM                                   ELGACL  
00607          PERFORM 0180-OBTAIN-BENEFIT-PROVISIONX THRU 0180-END.    ELGACL  
00608      IF SRP-NO-ACCUMS-FOUND                                       ELGACL  
00609          PERFORM 0190-DISPLAY-NO-ACCUMS-MESSAGE THRU 0190-END     ELGACL  
00610      ELSE IF SRP-INST-NOT-APPLICABLE OR                           ELGACL  
00611          SRP-PROF-NOT-APPLICABLE                                  ELGACL  
00612          PERFORM 0200-DISPLAY-NOT-APPLICABLE-LO THRU 0200-END     ELGACL  
00613      ELSE                                                         ELGACL  
00614          PERFORM 0250-DISPLAY-REGULAR-TEXT THRU 0250-END.         ELGACL  
00615      IF SRP-TOPIC-ACCUM                                           ELGACL  
00616          PERFORM 1000-END-THE-DISPLAY THRU 1000-END.              ELGACL  
00617    0170-END.                                                      ELGACL  
00618      EXIT.                                                        ELGACL  
00619      EJECT                                                        ELGACL  
00620                                                                   ELGACL  
00621                                                                   ELGACL  
00622 ************************************************************      ELGACL  
00623 *                                                          *      ELGACL  
00624 *        OBTAIN BENEFIT PROVISION HEADER                   *      ELGACL  
00625 *                                                          *      ELGACL  
00626 ************************************************************      ELGACL  
00627  0180-OBTAIN-BENEFIT-PROVISIONX.                                  ELGACL  
00628 *    *-----------------------------------------------------------*ELGACL  
00629 *    *  PERFORMED BY 0170-PROCESS.                               *ELGACL  
00630 *    *-----------------------------------------------------------*ELGACL  
00631      INITIALIZE COF-DTL-LINE (1).                                 ELGACL  
00632      MOVE BENEFIT-PROVISION-HEADER TO COF-DTL-LINE (2).           ELGACL  
00633      SET  COF-CONTINUE  TO  TRUE.                                 ELGACL  
00634      MOVE +0            TO  COF-NBR-HDR-LINES.                    ELGACL  
00635      MOVE  2            TO  COF-NBR-DTL-LINES.                    ELGACL  
00636      PERFORM 0970-SETUP-FOR-OUTPUT-LINK THRU 0970-END.            ELGACL  
00637    0180-END.                                                      ELGACL  
00638      EXIT.                                                        ELGACL  
00639                                                                   ELGACL  
00640                                                                   ELGACL  
00641 ************************************************************      ELGACL  
00642 *                                                          *      ELGACL  
00643 *        DISPLAY NO ACCUMS MESSAGE                         *      ELGACL  
00644 *                                                          *      ELGACL  
00645 ************************************************************      ELGACL  
00646  0190-DISPLAY-NO-ACCUMS-MESSAGE.                                  ELGACL  
00647 *    *-----------------------------------------------------------*ELGACL  
00648 *    *  PERFORMED BY 0170-PROCESS.                               *ELGACL  
00649 *    *-----------------------------------------------------------*ELGACL  
00650      ADD  +1             TO  TCAR-FROM-SUB.                       ELGACL  
00651      MOVE NO-ACCUMS-MSG  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).      ELGACL  
00652      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGACL  
00653      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGACL  
00654    0190-END.                                                      ELGACL  
00655      EXIT.                                                        ELGACL  
00656      EJECT                                                        ELGACL  
00657                                                                   ELGACL  
00658                                                                   ELGACL  
00659 ************************************************************      ELGACL  
00660 *                                                          *      ELGACL  
00661 *        DISPLAY NOT APPLICABLE LOB MESSAGE                *      ELGACL  
00662 *                                                          *      ELGACL  
00663 ************************************************************      ELGACL  
00664  0200-DISPLAY-NOT-APPLICABLE-LO.                                  ELGACL  
00665 *    *-----------------------------------------------------------*ELGACL  
00666 *    *  PERFORMED BY 0170-PROCESS.                               *ELGACL  
00667 *    *-----------------------------------------------------------*ELGACL  
00668      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
00669      IF SRP-INST-NOT-APPLICABLE                                   ELGACL  
00670         MOVE NO-INST-LOB-MSG  TO  TCAR-FROM-LINE (TCAR-FROM-SUB)  ELGACL  
00671      ELSE                                                         ELGACL  
00672         MOVE NO-PROF-LOB-MSG  TO  TCAR-FROM-LINE (TCAR-FROM-SUB). ELGACL  
00673      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGACL  
00674      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGACL  
00675    0200-END.                                                      ELGACL  
00676      EXIT.                                                        ELGACL  
00677                                                                   ELGACL  
00678                                                                   ELGACL  
00679 ************************************************************      ELGACL  
00680 *                                                          *      ELGACL  
00681 *        DISPLAY REGULAR TEXT                              *      ELGACL  
00682 *                                                          *      ELGACL  
00683 ************************************************************      ELGACL  
00684  0250-DISPLAY-REGULAR-TEXT.                                       ELGACL  
00685 *    *-----------------------------------------------------------*ELGACL  
00686 *    *  PERFORMED BY 0170-PROCESS.                               *ELGACL  
00687 *    *-----------------------------------------------------------*ELGACL  
00688      MOVE +1      TO  IOP-TSQ-ITEM-NBR.                           ELGACL  
00689      SET  IOP-RD  TO  TRUE.                                       ELGACL  
00690      PERFORM 0950-SETUP-AND-READ-FILE THRU 0950-END.              ELGACL  
00691      PERFORM 0260-DISPLAY-OCCURENCE-TEXT THRU 0260-END            ELGACL  
00692          UNTIL NOT IOP-RC-OK.                                     ELGACL  
00693      PERFORM 0980-DELETE-ACCUM-OCCURENCE-FI THRU 0980-END.        ELGACL  
00694    0250-END.                                                      ELGACL  
00695      EXIT.                                                        ELGACL  
00696      EJECT                                                        ELGACL  
00697                                                                   ELGACL  
00698                                                                   ELGACL  
00699 ************************************************************      ELGACL  
00700 *                                                          *      ELGACL  
00701 *        DISPLAY OCCURENCE TEXT                            *      ELGACL  
00702 *                                                          *      ELGACL  
00703 ************************************************************      ELGACL  
00704  0260-DISPLAY-OCCURENCE-TEXT.                                     ELGACL  
00705 *    *-----------------------------------------------------------*ELGACL  
00706 *    *  PERFORMED BY 0250-DISPLAY-REGULAR-TEXT.                  *ELGACL  
00707 *    *-----------------------------------------------------------*ELGACL  
00708      PERFORM 0270-PROCESS-PERCENT-PHRASE THRU 0270-END.           ELGACL  
00709      PERFORM 0500-CREATE-COINSURANCE-APPLIC THRU 0500-END.        ELGACL  
00710      IF NOT DEFINITION-NA                                         ELGACL  
00711          PERFORM 0800-CREATE-DEFINITION-SENTENC THRU 0800-END.    ELGACL  
00712      IF NOT BENEFIT-PERIOD-NA                                     ELGACL  
00713          PERFORM 0830-CREATE-BENEFIT-PERIOD-SEN THRU 0830-END.    ELGACL  
00714      IF WS-HAS-AN-INTERNAL                                        ELGACL  
00715          PERFORM 0750-CALL-INTERNALS THRU 0750-END.               ELGACL  
00716      IF SRP-TOPIC-ACCUM                                           ELGACL  
00717          PERFORM 0940-DISPLAY-COINSURANCE-END-S THRU 0940-END.    ELGACL  
00718      SET IOP-RD-NXT TO  TRUE.                                     ELGACL  
00719      PERFORM 0950-SETUP-AND-READ-FILE THRU 0950-END.              ELGACL  
00720      IF SRP-TOPIC-ACCUM AND IOP-RC-OK                             ELGACL  
00721          PERFORM 0960-OBTAIN-TOPIC-HEADER THRU 0960-END.          ELGACL  
00722    0260-END.                                                      ELGACL  
00723      EXIT.                                                        ELGACL  
00724      EJECT                                                        ELGACL  
00725                                                                   ELGACL  
00726                                                                   ELGACL  
00727 ************************************************************      ELGACL  
00728 *                                                          *      ELGACL  
00729 *        PROCESS PERCENT PHRASE                            *      ELGACL  
00730 *                                                          *      ELGACL  
00731 ************************************************************      ELGACL  
00732  0270-PROCESS-PERCENT-PHRASE.                                     ELGACL  
00733 *    *-----------------------------------------------------------*ELGACL  
00734 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGACL  
00735 *    *-----------------------------------------------------------*ELGACL  
00736      PERFORM 0730-INITIALIZE-SWITCHES THRU 0730-END.              ELGACL  
00737      PERFORM 0840-INITIALIZE-COMPRESS-AREA THRU 0840-END.         ELGACL  
00738      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGACL  
00739          PERFORM 0290-CREATE-SINGLE-PCENT-PHRA                    ELGACL  
00740                  THRU 0290-END                                    ELGACL  
00741      ELSE                                                         ELGACL  
00742          PERFORM 0300-CREATE-MULTI-PCENT-PHRA                     ELGACL  
00743                  THRU 0300-END.                                   ELGACL  
00744      PERFORM 0390-GET-FAM-OR-IND-PHRASE THRU 0390-END.            ELGACL  
00745      PERFORM 0410-GET-L-O-B-PHRA THRU 0410-END.                   ELGACL  
00746    0270-END.                                                      ELGACL  
00747      EXIT.                                                        ELGACL  
00748      EJECT                                                        ELGACL  
00749                                                                   ELGACL  
00750                                                                   ELGACL  
00751 ************************************************************      ELGACL  
00752 *                                                          *      ELGACL  
00753 *        CREATE SINGLE LEVEL PERCENT PHRASE                *      ELGACL  
00754 *                                                          *      ELGACL  
00755 ************************************************************      ELGACL  
00756  0290-CREATE-SINGLE-PCENT-PHRA.                                   ELGACL  
00757 *    *-----------------------------------------------------------*ELGACL  
00758 *    *  PERFORMED BY 0270-PROCESS-PERCENT-PHRASE.                *ELGACL  
00759 *    *-----------------------------------------------------------*ELGACL  
00760      INITIALIZE WS-VAL-LIMIT-AMOUNT                               ELGACL  
00761                 WS-SINGLE-VAL-LMT-PHR.                            ELGACL  
00762      MOVE ACCUM-PERCENT-LEVEL (1) TO WS-SINGLE-PERCENT-LEVEL.     ELGACL  
00763      SET ASC-DES-INDEX TO 1.                                      ELGACL  
00764      PERFORM 0330-PROCESS-VALUE-LIMIT THRU 0330-END.              ELGACL  
00765    0290-END.                                                      ELGACL  
00766      EXIT.                                                        ELGACL  
00767      EJECT                                                        ELGACL  
00768                                                                   ELGACL  
00769                                                                   ELGACL  
00770 ************************************************************      ELGACL  
00771 *                                                          *      ELGACL  
00772 *        CREATE MULTI LEVEL PERCENT PHRASE                 *      ELGACL  
00773 *                                                          *      ELGACL  
00774 ************************************************************      ELGACL  
00775  0300-CREATE-MULTI-PCENT-PHRA.                                    ELGACL  
00776 *    *-----------------------------------------------------------*ELGACL  
00777 *    *  PERFORMED BY 0270-PROCESS-PERCENT-PHRASE.                *ELGACL  
00778 *    *-----------------------------------------------------------*ELGACL  
00779      IF COF-NBR-DTL-LINES > 0                                     ELGACL  
00780         PERFORM 1100-LINK-TO-OUTPUT-MODULE THRU 1100-END.         ELGACL  
00781      ADD +1 TO TCAR-FROM-SUB.                                     ELGACL  
00782      MOVE PC-MULTI-PCENT-START TO                                 ELGACL  
00783           TCAR-FROM-LINE(TCAR-FROM-SUB).                          ELGACL  
00784      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGACL  
00785      SET ASC-DES-INDEX TO 1.                                      ELGACL  
00786      MOVE SPACES TO WS-MVL-CHANGES-PHRASE.                        ELGACL  
00787      PERFORM 0310-CREATE-PCENT-VALUE-PHRA THRU 0310-END.          ELGACL  
00788      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGACL  
00789      MOVE WS-MULTI-VALUE-LINE TO COF-DTL-LINE (COF-NBR-DTL-LINES).ELGACL  
00790      IF COF-NBR-DTL-LINES >= 20                                   ELGACL  
00791         PERFORM 1100-LINK-TO-OUTPUT-MODULE THRU 1100-END          ELGACL  
00792         INITIALIZE COF-DTL                                        ELGACL  
00793         MOVE ZERO TO COF-NBR-DTL-LINES.                           ELGACL  
00794      PERFORM 0320-ASCEND-DESCEND-LOOP THRU 0320-END               ELGACL  
00795              UNTIL ASC-DES-INDEX = ACCUM-ASCEND-DESCEND-COUNT.    ELGACL  
00796      IF COF-NBR-DTL-LINES > 0                                     ELGACL  
00797         PERFORM 1100-LINK-TO-OUTPUT-MODULE THRU 1100-END.         ELGACL  
00798      PERFORM 0840-INITIALIZE-COMPRESS-AREA THRU 0840-END.         ELGACL  
00799    0300-END.                                                      ELGACL  
00800      EXIT.                                                        ELGACL  
00801      EJECT                                                        ELGACL  
00802                                                                   ELGACL  
00803                                                                   ELGACL  
00804 ************************************************************      ELGACL  
00805 *                                                          *      ELGACL  
00806 *        CREATE PERCENT VALUE PHRASE                       *      ELGACL  
00807 *                                                          *      ELGACL  
00808 ************************************************************      ELGACL  
00809  0310-CREATE-PCENT-VALUE-PHRA.                                    ELGACL  
00810 *    *-----------------------------------------------------------*ELGACL  
00811 *    *  PERFORMED BY 0300-CREATE-MULTI-PCENT-PHRA                *ELGACL  
00812 *    *               0320-ASCEND-DESCEND-LOOP.                   *ELGACL  
00813 *    *-----------------------------------------------------------*ELGACL  
00814      MOVE 1 TO TCAR-FROM-SUB.                                     ELGACL  
00815      MOVE ACCUM-PERCENT-LEVEL (ASC-DES-INDEX) TO                  ELGACL  
00816           WS-MULTI-PERCENT-LEVEL.                                 ELGACL  
00817      PERFORM 0330-PROCESS-VALUE-LIMIT THRU 0330-END.              ELGACL  
00818      MOVE PC-DOTS TO WS-MVL-MASK.                                 ELGACL  
00819      PERFORM 0700-SEND-PART-PARA THRU 0700-END.                   ELGACL  
00820      STRING TCAR-FROM-LINE (1)                                    ELGACL  
00821         DELIMITED BY '  ' INTO WS-MVL-MASK.                       ELGACL  
00822      SET VLT-INDEX TO ASC-DES-INDEX.                              ELGACL  
00823      MOVE VLT-VARIABLE-LEVEL-TAG (VLT-INDEX) TO                   ELGACL  
00824           WS-MVL-LEVEL-TAG.                                       ELGACL  
00825    0310-END.                                                      ELGACL  
00826      EXIT.                                                        ELGACL  
00827      EJECT                                                        ELGACL  
00828                                                                   ELGACL  
00829                                                                   ELGACL  
00830 ************************************************************      ELGACL  
00831 *                                                          *      ELGACL  
00832 *        ASCEND-DESCEND-LOOP                               *      ELGACL  
00833 *                                                          *      ELGACL  
00834 ************************************************************      ELGACL  
00835  0320-ASCEND-DESCEND-LOOP.                                        ELGACL  
00836 *    *-----------------------------------------------------------*ELGACL  
00837 *    *  PERFORMED BY 0300-CREATE-MULTI-PCENT-PHRA.               *ELGACL  
00838 *    *-----------------------------------------------------------*ELGACL  
00839      MOVE PC-CHANGES-TO TO WS-MVL-CHANGES-PHRASE.                 ELGACL  
00840      SET ASC-DES-INDEX UP BY 1.                                   ELGACL  
00841      PERFORM 0310-CREATE-PCENT-VALUE-PHRA THRU 0310-END.          ELGACL  
00842      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGACL  
00843      MOVE WS-MULTI-VALUE-LINE                                     ELGACL  
00844        TO COF-DTL-LINE (COF-NBR-DTL-LINES).                       ELGACL  
00845      IF COF-NBR-DTL-LINES >= 20                                   ELGACL  
00846         PERFORM 1100-LINK-TO-OUTPUT-MODULE THRU 1100-END          ELGACL  
00847         INITIALIZE COF-DTL                                        ELGACL  
00848         MOVE ZERO TO COF-NBR-DTL-LINES.                           ELGACL  
00849    0320-END.                                                      ELGACL  
00850      EXIT.                                                        ELGACL  
00851      EJECT                                                        ELGACL  
00852                                                                   ELGACL  
00853                                                                   ELGACL  
00854 ************************************************************      ELGACL  
00855 *                                                          *      ELGACL  
00856 *        PROCESS VALUE LIMIT                               *      ELGACL  
00857 *                                                          *      ELGACL  
00858 ************************************************************      ELGACL  
00859  0330-PROCESS-VALUE-LIMIT.                                        ELGACL  
00860 *    *-----------------------------------------------------------*ELGACL  
00861 *    *  PERFORMED BY 0290-CREATE-SINGLE-PCENT-PHRA.              *ELGACL  
00862 *    *  PERFORMED BY 0310-CREATE-PCENT-VALUE-PHRA.               *ELGACL  
00863 *    *-----------------------------------------------------------*ELGACL  
00864 ******************************************************************ELGACL  
00865 *   THE COINSURANCE OTHER SOURCE INDICATOR IS NOT DEFINED IN THE  ELGACL  
00866 * CODES MANUAL AS OF JUNE 12, 1992.  FOR THIS REASON IT CANNOT BE ELGACL  
00867 * TRANSLATED SO A MESSAGE IS DISPLAYED IF THE ACCUM VALUE LIMIT   ELGACL  
00868 * IS A NEGATIVE NUMBER (INDICATING OTHER SOURCE).                 ELGACL  
00869 ******************************************************************ELGACL  
00870      IF ACCUM-VALUE-LIMIT (ASC-DES-INDEX) < 0                     ELGACL  
00871         MOVE COINSURANCE-OTHR-SRCE-MSG TO WS-VAL-QUAL-UNLIMITED   ELGACL  
00872      ELSE                                                         ELGACL  
00873         PERFORM 0340-GET-VALUE-LIMIT-QUALIFIER THRU 0340-END.     ELGACL  
00874                                                                   ELGACL  
00875      IF ACCUM-ASCEND-DESCEND-COUNT = 1                            ELGACL  
00876         MOVE WS-VAL-LIMIT-AMOUNT TO WS-SINGLE-VAL-LMT-PHR         ELGACL  
00877         ADD 1 TO TCAR-FROM-SUB                                    ELGACL  
00878         MOVE WS-SINGLE-PERCENT-PHRASE                             ELGACL  
00879           TO TCAR-FROM-LINE (TCAR-FROM-SUB)                       ELGACL  
00880      ELSE                                                         ELGACL  
00881         MOVE WS-VAL-LIMIT-AMOUNT TO WS-MULTI-VAL-LMT-PHR          ELGACL  
00882         MOVE WS-MULTI-VALUE-PHRASE                                ELGACL  
00883           TO TCAR-FROM-LINE (TCAR-FROM-SUB)                       ELGACL  
00884      END-IF.                                                      ELGACL  
00885                                                                   ELGACL  
00886      PERFORM 0740-CHECK-FOR-INTERNALS THRU 0740-END.              ELGACL  
00887    0330-END.                                                      ELGACL  
00888      EXIT.                                                        ELGACL  
00889                                                                   ELGACL  
00890                                                                   ELGACL  
00891                                                                   ELGACL  
00892                                                                   ELGACL  
00893 ************************************************************      ELGACL  
00894 *                                                          *      ELGACL  
00895 *        GET VALUE LIMIT QUALIFIER PHRASE                  *      ELGACL  
00896 *                                                          *      ELGACL  
00897 ************************************************************      ELGACL  
00898 *    *-----------------------------------------------------------*ELGACL  
00899 *    *  PERFORMED BY 0330-PROCESS-VALUE-LIMIT                    *ELGACL  
00900 *    *-----------------------------------------------------------*ELGACL  
00901  0340-GET-VALUE-LIMIT-QUALIFIER.                                  ELGACL  
00902      MOVE ACCUM-VALUE-QUALIFIER   TO  CMF-CODE-VALUE.             ELGACL  
00903      MOVE 'COINS-VALUE-QUALIFIER' TO  CMF-ELEMENT-SYSTEM-NAME.    ELGACL  
00904      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
00905                                                                   ELGACL  
00906      IF CMF-DESCR-LINE (1)  =  PC-DOLLARS                         ELGACL  
00907         PERFORM 0370-BUILD-DOLLAR-VAL-LIM-PHRA THRU 0370-END      ELGACL  
00908      ELSE PERFORM 0380-BUILD-OTHER-VAL-LIM-PHRA                   ELGACL  
00909                   THRU 0380-END.                                  ELGACL  
00910    0340-END.                                                      ELGACL  
00911      EXIT.                                                        ELGACL  
00912                                                                   ELGACL  
00913                                                                   ELGACL  
00914 ************************************************************      ELGACL  
00915 *                                                          *      ELGACL  
00916 *        BUILD DOLLAR VALUE LIMIT PHRASE                   *      ELGACL  
00917 *                                                          *      ELGACL  
00918 ************************************************************      ELGACL  
00919 *    *-----------------------------------------------------------*ELGACL  
00920 *    *  PERFORMED BY 0360-CHECK-LIMIT-TYPE                       *ELGACL  
00921 *    *-----------------------------------------------------------*ELGACL  
00922  0370-BUILD-DOLLAR-VAL-LIM-PHRA.                                  ELGACL  
00923      MOVE SPACES TO WS-VAL-QUAL-UNLIMITED.                        ELGACL  
00924      MOVE ACCUM-VALUE-LIMIT (ASC-DES-INDEX)                       ELGACL  
00925           TO WS-VAL-LIM-DOLLARS.                                  ELGACL  
00926      IF WS-MAX-LIMIT-DOLLARS                                      ELGACL  
00927         STRING 'UNLIMITED ' DELIMITED BY SIZE, CMF-DESCR-LINE (1) ELGACL  
00928           DELIMITED BY '  ',                                      ELGACL  
00929           ' ' DELIMITED BY SIZE                                   ELGACL  
00930           INTO WS-VAL-QUAL-UNLIMITED                              ELGACL  
00931      ELSE CONTINUE.                                               ELGACL  
00932   0370-END. EXIT.                                                 ELGACL  
00933                                                                   ELGACL  
00934                                                                   ELGACL  
00935 ************************************************************      ELGACL  
00936 *                                                          *      ELGACL  
00937 *        BUILD OTHER VALUE LIMIT PHRASE                    *      ELGACL  
00938 *                                                          *      ELGACL  
00939 ************************************************************      ELGACL  
00940 *    *-----------------------------------------------------------*ELGACL  
00941 *    *  PERFORMED BY 0360-CHECK-LIMIT-TYPE                       *ELGACL  
00942 *    *-----------------------------------------------------------*ELGACL  
00943  0380-BUILD-OTHER-VAL-LIM-PHRA.                                   ELGACL  
00944      MOVE ACCUM-VALUE-LIMIT-NON-DOLLAR (ASC-DES-INDEX)            ELGACL  
00945         TO WS-VAL-LIM-OTHER.                                      ELGACL  
00946      MOVE SPACES TO WS-VAL-QUAL-OTHER.                            ELGACL  
00947      IF WS-MAX-LIMIT-OTHER                                        ELGACL  
00948         STRING ' UNLIMITED ' DELIMITED BY SIZE, CMF-DESCR-LINE (1)ELGACL  
00949           DELIMITED BY '  ',                                      ELGACL  
00950           ' ' DELIMITED BY SIZE                                   ELGACL  
00951           INTO WS-VAL-QUAL-UNLIMITED                              ELGACL  
00952      ELSE                                                         ELGACL  
00953           STRING ' ' DELIMITED BY SIZE                            ELGACL  
00954                  CMF-DESCR-LINE (1) DELIMITED BY '  '             ELGACL  
00955                INTO WS-VAL-QUAL-OTHER.                            ELGACL  
00956   0380-END. EXIT.                                                 ELGACL  
00957                                                                   ELGACL  
00958                                                                   ELGACL  
00959                                                                   ELGACL  
00960 ************************************************************      ELGACL  
00961 *                                                          *      ELGACL  
00962 *        GET FAMILY OR INDIVIDUAL PHRASE                   *      ELGACL  
00963 *                                                          *      ELGACL  
00964 ************************************************************      ELGACL  
00965  0390-GET-FAM-OR-IND-PHRASE.                                      ELGACL  
00966 *    *-----------------------------------------------------------*ELGACL  
00967 *    *  PERFORMED BY 0280-DISPLAY-COMMON-BODY.                   *ELGACL  
00968 *    *-----------------------------------------------------------*ELGACL  
00969      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
00970      MOVE PC-PER TO TCAR-FROM-LINE (TCAR-FROM-SUB).               ELGACL  
00971      PERFORM 0400-TRANSLATE-FAMILY-OR-INDIV THRU 0400-END.        ELGACL  
00972      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGACL  
00973    0390-END.                                                      ELGACL  
00974      EXIT.                                                        ELGACL  
00975                                                                   ELGACL  
00976                                                                   ELGACL  
00977 ************************************************************      ELGACL  
00978 *                                                          *      ELGACL  
00979 *        TRANSLATE FAMILY OR INDIVIDUAL                    *      ELGACL  
00980 *                                                          *      ELGACL  
00981 ************************************************************      ELGACL  
00982  0400-TRANSLATE-FAMILY-OR-INDIV.                                  ELGACL  
00983 *    *-----------------------------------------------------------*ELGACL  
00984 *    *  PERFORMED BY 0390-GET-FAMILY-OR-INDIVIDUALX.             *ELGACL  
00985 *    *-----------------------------------------------------------*ELGACL  
00986      MOVE ACCUM-FAM-OR-INDIV   TO  CMF-CODE-VALUE.                ELGACL  
00987      MOVE 'COINS-FAM-OR-INDIV' TO  CMF-ELEMENT-SYSTEM-NAME.       ELGACL  
00988      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
00989    0400-END.                                                      ELGACL  
00990      EXIT.                                                        ELGACL  
00991      EJECT                                                        ELGACL  
00992                                                                   ELGACL  
00993                                                                   ELGACL  
00994 ************************************************************      ELGACL  
00995 *                                                          *      ELGACL  
00996 *        GET LINE OF BUSINESS PHRASE                       *      ELGACL  
00997 *                                                          *      ELGACL  
00998 ************************************************************      ELGACL  
00999  0410-GET-L-O-B-PHRA.                                             ELGACL  
01000 *    *-----------------------------------------------------------*ELGACL  
01001 *    *  PERFORMED BY 0280-DISPLAY-COMMON-BODY.                   *ELGACL  
01002 *    *-----------------------------------------------------------*ELGACL  
01003      ADD  +1      TO  TCAR-FROM-SUB.                              ELGACL  
01004      MOVE PC-FOR  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGACL  
01005      PERFORM 0420-TRANSLATE-LINE-OF-BUSINES THRU 0420-END.        ELGACL  
01006      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGACL  
01007      ADD  +1                    TO  TCAR-FROM-SUB.                ELGACL  
01008      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGACL  
01009      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGACL  
01010      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGACL  
01011    0410-END.                                                      ELGACL  
01012      EXIT.                                                        ELGACL  
01013                                                                   ELGACL  
01014                                                                   ELGACL  
01015 ************************************************************      ELGACL  
01016 *                                                          *      ELGACL  
01017 *        TRANSLATE LINE OF BUSINESS                        *      ELGACL  
01018 *                                                          *      ELGACL  
01019 ************************************************************      ELGACL  
01020  0420-TRANSLATE-LINE-OF-BUSINES.                                  ELGACL  
01021 *    *-----------------------------------------------------------*ELGACL  
01022 *    *  PERFORMED BY 0410-GET-L-O-B-PHRA.                        *ELGACL  
01023 *    *-----------------------------------------------------------*ELGACL  
01024      MOVE ACCUM-L-O-B   TO  CMF-CODE-VALUE.                       ELGACL  
01025      MOVE 'COINS-L-O-B' TO  CMF-ELEMENT-SYSTEM-NAME.              ELGACL  
01026      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01027    0420-END.                                                      ELGACL  
01028      EXIT.                                                        ELGACL  
01029      EJECT                                                        ELGACL  
01030                                                                   ELGACL  
01031                                                                   ELGACL  
01032 ************************************************************      ELGACL  
01033 *                                                          *      ELGACL  
01034 *        CREATE COINSURANCE APPLICABILITY SENTENCE         *      ELGACL  
01035 *                                                          *      ELGACL  
01036 ************************************************************      ELGACL  
01037  0500-CREATE-COINSURANCE-APPLIC.                                  ELGACL  
01038 *    *-----------------------------------------------------------*ELGACL  
01039 *    *  PERFORMED BY 0260-DISPLAY-OCCURRENCE-TEXT                *ELGACL  
01040 *    *-----------------------------------------------------------*ELGACL  
01041      ADD   +1      TO  TCAR-FROM-SUB.                             ELGACL  
01042      MOVE PC-THIS  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGACL  
01043      IF  FYI-VALUE-NA                                             ELGACL  
01044          CONTINUE                                                 ELGACL  
01045      ELSE PERFORM 0520-GET-FYI-PHRASE THRU 0520-END.              ELGACL  
01046      ADD  +1                    TO  TCAR-FROM-SUB.                ELGACL  
01047      MOVE   PC-COINSURANCE-APPLIES                                ELGACL  
01048             TO TCAR-FROM-LINE (TCAR-FROM-SUB).                    ELGACL  
01049      ADD  +1                    TO  TCAR-FROM-SUB.                ELGACL  
01050      IF COST-CONTAIN-IND-NA                                       ELGACL  
01051          ADD  +1               TO  TCAR-FROM-SUB                  ELGACL  
01052          MOVE PC-SERVICES      TO  TCAR-FROM-LINE (TCAR-FROM-SUB) ELGACL  
01053      ELSE                                                         ELGACL  
01054          PERFORM 0560-GET-COST-CONTAINMENT-PHRA THRU 0560-END.    ELGACL  
01055      ADD  +1                TO  TCAR-FROM-SUB.                    ELGACL  
01056      MOVE PC-FOR  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGACL  
01057      PERFORM 0600-GET-CONDITION-BITS-PHRASE THRU 0600-END.        ELGACL  
01058      ADD  +1           TO  TCAR-FROM-SUB.                         ELGACL  
01059      MOVE PC-PROVIDED  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).        ELGACL  
01060      IF PLACE-OF-TREATMENT-NA                                     ELGACL  
01061         CONTINUE                                                  ELGACL  
01062      ELSE                                                         ELGACL  
01063          PERFORM 0620-GET-PLACE-OF-TREATMENT-PH THRU 0620-END.    ELGACL  
01064      EVALUATE    AGE-LMT-FROM-IND-NA                              ELGACL  
01065             ALSO AGE-LMT-TO-IND-NA                                ELGACL  
01066             ALSO RELATIONSHIP-IND-NA                              ELGACL  
01067         WHEN TRUE ALSO TRUE ALSO TRUE                             ELGACL  
01068            CONTINUE                                               ELGACL  
01069         WHEN TRUE ALSO TRUE ALSO FALSE                            ELGACL  
01070           PERFORM 0670-GET-RELATSHIP-IND-PHR THRU 0670-END        ELGACL  
01071         WHEN OTHER                                                ELGACL  
01072           PERFORM 0640-GET-AGE-AND-REL-PHR THRU 0640-END          ELGACL  
01073         END-EVALUATE.                                             ELGACL  
01074                                                                   ELGACL  
01075      IF WS-HAS-AN-INTERNAL                                        ELGACL  
01076         PERFORM 0760-GET-INTERNAL-TABULARS-LIS THRU 0760-END.     ELGACL  
01077      ADD  +1                    TO  TCAR-FROM-SUB.                ELGACL  
01078      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGACL  
01079      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGACL  
01080      PERFORM 0720-INSERT-BLANK-LINE THRU 0720-END.                ELGACL  
01081    0500-END.                                                      ELGACL  
01082      EXIT.                                                        ELGACL  
01083      EJECT                                                        ELGACL  
01084                                                                   ELGACL  
01085                                                                   ELGACL  
01086 ************************************************************      ELGACL  
01087 *                                                          *      ELGACL  
01088 *        GET FYI PHRASE                                    *      ELGACL  
01089 *                                                          *      ELGACL  
01090 ************************************************************      ELGACL  
01091  0520-GET-FYI-PHRASE.                                             ELGACL  
01092 *    *-----------------------------------------------------------*ELGACL  
01093 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGACL  
01094 *    *-----------------------------------------------------------*ELGACL  
01095      PERFORM 0530-TRANSLATE-FYI-VALUE THRU 0530-END.              ELGACL  
01096      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGACL  
01097    0520-END.                                                      ELGACL  
01098      EXIT.                                                        ELGACL  
01099                                                                   ELGACL  
01100                                                                   ELGACL  
01101 ************************************************************      ELGACL  
01102 *                                                          *      ELGACL  
01103 *        TRANSLATE FYI VALUE                               *      ELGACL  
01104 *                                                          *      ELGACL  
01105 ************************************************************      ELGACL  
01106  0530-TRANSLATE-FYI-VALUE.                                        ELGACL  
01107 *    *-----------------------------------------------------------*ELGACL  
01108 *    *  PERFORMED BY 0520-GET-FYI-PHRASE.                        *ELGACL  
01109 *    *-----------------------------------------------------------*ELGACL  
01110      MOVE ACCUM-FYI-VALUE   TO  CMF-CODE-VALUE.                   ELGACL  
01111      MOVE 'COINS-FYI-VALUE' TO                                    ELGACL  
01112          CMF-ELEMENT-SYSTEM-NAME.                                 ELGACL  
01113      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01114    0530-END.                                                      ELGACL  
01115      EXIT.                                                        ELGACL  
01116      EJECT                                                        ELGACL  
01117                                                                   ELGACL  
01118                                                                   ELGACL  
01119 ************************************************************      ELGACL  
01120 *                                                          *      ELGACL  
01121 *        GET COST CONTAINMENT PHRASE                       *      ELGACL  
01122 *                                                          *      ELGACL  
01123 ************************************************************      ELGACL  
01124  0560-GET-COST-CONTAINMENT-PHRA.                                  ELGACL  
01125 *    *-----------------------------------------------------------*ELGACL  
01126 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGACL  
01127 *    *-----------------------------------------------------------*ELGACL  
01128      PERFORM 0570-TRANSLATE-COST-CONTAINMEN THRU 0570-END.        ELGACL  
01129      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGACL  
01130    0560-END.                                                      ELGACL  
01131      EXIT.                                                        ELGACL  
01132                                                                   ELGACL  
01133                                                                   ELGACL  
01134 ************************************************************      ELGACL  
01135 *                                                          *      ELGACL  
01136 *        TRANSLATE COST CONTAINMENT IND                    *      ELGACL  
01137 *                                                          *      ELGACL  
01138 ************************************************************      ELGACL  
01139  0570-TRANSLATE-COST-CONTAINMEN.                                  ELGACL  
01140 *    *-----------------------------------------------------------*ELGACL  
01141 *    *  PERFORMED BY 0560-GET-COST-CONTAINMENT-PHRA.             *ELGACL  
01142 *    *-----------------------------------------------------------*ELGACL  
01143      MOVE ACCUM-COST-CONTAIN-IND   TO  CMF-CODE-VALUE.            ELGACL  
01144      MOVE 'COINS-COST-CONTAIN-IND' TO  CMF-ELEMENT-SYSTEM-NAME.   ELGACL  
01145      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01146    0570-END.                                                      ELGACL  
01147      EXIT.                                                        ELGACL  
01148      EJECT                                                        ELGACL  
01149                                                                   ELGACL  
01150                                                                   ELGACL  
01151 ************************************************************      ELGACL  
01152 *                                                          *      ELGACL  
01153 *        GET CONDITION BITS PHRASE                         *      ELGACL  
01154 *                                                          *      ELGACL  
01155 ************************************************************      ELGACL  
01156  0600-GET-CONDITION-BITS-PHRASE.                                  ELGACL  
01157 *    *-----------------------------------------------------------*ELGACL  
01158 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGACL  
01159 *    *-----------------------------------------------------------*ELGACL  
01160 * -- PRESERVE CONTENTS OF TCAR-FROM-AREA                          ELGACL  
01161 *    (ELUCONDB USES THE TEXT COMPRESSION WORK AREA, WHICH CAUSES  ELGACL  
01162 *    THE LOSS OF ANYTHING IN TCAR-FROM-AREA.  WE MUST SAVE THE    ELGACL  
01163 *    CURRENT CONTENT OF TCAR-FROM-AREA AND RESTORE IT AFTER       ELGACL  
01164 *    OBTAINING THE TRANSLATION OF THE CONDITION BITS.)            ELGACL  
01165      MOVE TCAR-FROM-SUB TO WS-TEXT-HOLD-COUNT.                    ELGACL  
01166      MOVE TCAR-FROM-AREA TO WS-TEXT-HOLD-TEXT.                    ELGACL  
01167                                                                   ELGACL  
01168 * -- GET CONDITION BIT TRANSLATION                                ELGACL  
01169      MOVE ACCUM-CONDITION  TO  CMF-CONDITION-BITS.                ELGACL  
01170      CALL 'ELUCONDB' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACL  
01171                                                                   ELGACL  
01172      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGACL  
01173      CALL 'ELUSETAD'                                              ELGACL  
01174         USING DFHCOMMAREA                                         ELGACL  
01175               ADDRESS OF CMF-DESCR.                               ELGACL  
01176                                                                   ELGACL  
01177 * -- RESTORE THE CONTENTS OF TCAR-FROM-AREA                       ELGACL  
01178      MOVE WS-TEXT-HOLD-COUNT TO TCAR-FROM-SUB.                    ELGACL  
01179      MOVE WS-TEXT-HOLD-TEXT TO TCAR-FROM-AREA.                    ELGACL  
01180                                                                   ELGACL  
01181 * -- APPEND THE TRANSLATION                                       ELGACL  
01182      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGACL  
01183    0600-END.                                                      ELGACL  
01184      EXIT.                                                        ELGACL  
01185                                                                   ELGACL  
01186                                                                   ELGACL  
01187 ************************************************************      ELGACL  
01188 *                                                          *      ELGACL  
01189 *        GET PLACE OF TREATMENT PHRASE                     *      ELGACL  
01190 *                                                          *      ELGACL  
01191 ************************************************************      ELGACL  
01192  0620-GET-PLACE-OF-TREATMENT-PH.                                  ELGACL  
01193 *    *-----------------------------------------------------------*ELGACL  
01194 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGACL  
01195 *    *-----------------------------------------------------------*ELGACL  
01196      PERFORM 0630-TRANSLATE-PLACE-OF-TREATM THRU 0630-END.        ELGACL  
01197      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGACL  
01198    0620-END.                                                      ELGACL  
01199      EXIT.                                                        ELGACL  
01200                                                                   ELGACL  
01201                                                                   ELGACL  
01202 ************************************************************      ELGACL  
01203 *                                                          *      ELGACL  
01204 *        TRANSLATE PLACE OF TREATMENT                      *      ELGACL  
01205 *                                                          *      ELGACL  
01206 ************************************************************      ELGACL  
01207  0630-TRANSLATE-PLACE-OF-TREATM.                                  ELGACL  
01208 *    *-----------------------------------------------------------*ELGACL  
01209 *    *  PERFORMED BY 0620-GET-PLACE-OF-TREATMENT-PH.             *ELGACL  
01210 *    *-----------------------------------------------------------*ELGACL  
01211      MOVE ACCUM-PLACE-OF-TREATMENT  TO  CMF-CODE-VALUE.           ELGACL  
01212      MOVE 'COINS-PLACE-OF-TREATMENT' TO CMF-ELEMENT-SYSTEM-NAME.  ELGACL  
01213      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01214    0630-END.                                                      ELGACL  
01215      EXIT.                                                        ELGACL  
01216                                                                   ELGACL  
01217                                                                   ELGACL  
01218 /***********************************************************      ELGACL  
01219 *                                                          *      ELGACL  
01220 *        GET AGE AND REL PHR                               *      ELGACL  
01221 *                                                          *      ELGACL  
01222 ************************************************************      ELGACL  
01223  0640-GET-AGE-AND-REL-PHR.                                        ELGACL  
01224 *    *-----------------------------------------------------------*ELGACL  
01225 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGACL  
01226 *    *-----------------------------------------------------------*ELGACL  
01227      IF   RELATIONSHIP-IND-NA                                     ELGACL  
01228           ADD +1 TO TCAR-FROM-SUB                                 ELGACL  
01229           MOVE 'PATIENTS' TO TCAR-FROM-LINE (TCAR-FROM-SUB)       ELGACL  
01230      ELSE                                                         ELGACL  
01231           PERFORM 0670-GET-RELATSHIP-IND-PHR THRU 0670-END        ELGACL  
01232      ADD +1 TO TCAR-FROM-SUB.                                     ELGACL  
01233      MOVE 'FROM AGE' TO TCAR-FROM-LINE (TCAR-FROM-SUB).           ELGACL  
01234      PERFORM 0650-GET-FROM-AGE-PHRASE THRU 0650-END.              ELGACL  
01235      ADD +1 TO TCAR-FROM-SUB.                                     ELGACL  
01236      MOVE 'TO AGE' TO  TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGACL  
01237      PERFORM 0660-GET-TO-AGE-PHRASE THRU 0660-END.                ELGACL  
01238    0640-END.                                                      ELGACL  
01239      EXIT.                                                        ELGACL  
01240                                                                   ELGACL  
01241                                                                   ELGACL  
01242 /***********************************************************      ELGACL  
01243 *                                                          *      ELGACL  
01244 *        GET FROM AGE PHRASE                               *      ELGACL  
01245 *                                                          *      ELGACL  
01246 ************************************************************      ELGACL  
01247  0650-GET-FROM-AGE-PHRASE.                                        ELGACL  
01248 *    *-----------------------------------------------------------*ELGACL  
01249 *    *  PERFORMED BY 0640-GET-AGE-AND-REL-PHR.                   *ELGACL  
01250 *    *-----------------------------------------------------------*ELGACL  
01251      IF   ACCUM-AGE-LIMIT-FROM-VAL = +999                         ELGACL  
01252           MOVE  PC-UNLIMITED            TO  WS-FROM-AGE-ALPHA     ELGACL  
01253      ELSE                                                         ELGACL  
01254           MOVE ACCUM-AGE-LIMIT-FROM-VAL TO  WS-FROM-AGE.          ELGACL  
01255                                                                   ELGACL  
01256      MOVE ACCUM-AGE-LIMIT-FROM-IND  TO  CMF-CODE-VALUE.           ELGACL  
01257      MOVE 'COINS-AGE-QUAL-IND-FROM' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGACL  
01258      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01259                                                                   ELGACL  
01260      MOVE CMF-DESCR-LINE (1)  TO  WS-FROM-AGE-QUAL.               ELGACL  
01261      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
01262      MOVE WS-FROM-AGE-QUAL-PHRASE                                 ELGACL  
01263               TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                 ELGACL  
01264    0650-END.                                                      ELGACL  
01265      EXIT.                                                        ELGACL  
01266                                                                   ELGACL  
01267                                                                   ELGACL  
01268 /***********************************************************      ELGACL  
01269 *                                                          *      ELGACL  
01270 *        GET TO AGE PHRASE                                 *      ELGACL  
01271 *                                                          *      ELGACL  
01272 ************************************************************      ELGACL  
01273  0660-GET-TO-AGE-PHRASE.                                          ELGACL  
01274 *    *-----------------------------------------------------------*ELGACL  
01275 *    *  PERFORMED BY 0640-GET-AGE-AND-REL-PHR.                   *ELGACL  
01276 *    *-----------------------------------------------------------*ELGACL  
01277      IF   ACCUM-AGE-LIMIT-TO-VAL = +999                           ELGACL  
01278           MOVE  PC-UNLIMITED            TO  WS-TO-AGE-ALPHA       ELGACL  
01279      ELSE                                                         ELGACL  
01280           MOVE ACCUM-AGE-LIMIT-TO-VAL TO    WS-TO-AGE.            ELGACL  
01281                                                                   ELGACL  
01282      MOVE ACCUM-AGE-LIMIT-TO-IND    TO  CMF-CODE-VALUE.           ELGACL  
01283      MOVE 'COINS-AGE-QUAL-IND-TO'   TO  CMF-ELEMENT-SYSTEM-NAME.  ELGACL  
01284      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01285                                                                   ELGACL  
01286      MOVE CMF-DESCR-LINE (1)  TO  WS-TO-AGE-QUAL.                 ELGACL  
01287      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
01288      MOVE WS-TO-AGE-QUAL-PHRASE                                   ELGACL  
01289               TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                 ELGACL  
01290    0660-END.                                                      ELGACL  
01291      EXIT.                                                        ELGACL  
01292                                                                   ELGACL  
01293 ************************************************************      ELGACL  
01294 *                                                          *      ELGACL  
01295 *        GET RELATIONSHIP INDICATOR PHRASE                 *      ELGACL  
01296 *                                                          *      ELGACL  
01297 ************************************************************      ELGACL  
01298  0670-GET-RELATSHIP-IND-PHR.                                      ELGACL  
01299 *    *-----------------------------------------------------------*ELGACL  
01300 *    *  PERFORMED BY 0500-CREATE-APPLICABILITY-PHRA.             *ELGACL  
01301 *    *-----------------------------------------------------------*ELGACL  
01302      ADD  +1           TO  TCAR-FROM-SUB.                         ELGACL  
01303      MOVE PC-TO        TO  TCAR-FROM-LINE (TCAR-FROM-SUB).        ELGACL  
01304      PERFORM 0680-TRANSLATE-RELATIONSHIP    THRU 0680-END.        ELGACL  
01305      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGACL  
01306    0670-END.                                                      ELGACL  
01307      EXIT.                                                        ELGACL  
01308                                                                   ELGACL  
01309                                                                   ELGACL  
01310 ************************************************************      ELGACL  
01311 *                                                          *      ELGACL  
01312 *        TRANSLATE RELATIONSHIP                            *      ELGACL  
01313 *                                                          *      ELGACL  
01314 ************************************************************      ELGACL  
01315  0680-TRANSLATE-RELATIONSHIP.                                     ELGACL  
01316 *    *-----------------------------------------------------------*ELGACL  
01317 *    *  PERFORMED BY 0670-GET-RELATIONSHIP-IND-PHR.              *ELGACL  
01318 *    *-----------------------------------------------------------*ELGACL  
01319      MOVE ACCUM-RELATIONSHIP-IND    TO  CMF-CODE-VALUE.           ELGACL  
01320      MOVE 'COINS-RELATIONSHIP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.  ELGACL  
01321      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01322    0680-END.                                                      ELGACL  
01323      EXIT.                                                        ELGACL  
01324                                                                   ELGACL  
01325                                                                   ELGACL  
01326 ************************************************************      ELGACL  
01327 *                                                          *      ELGACL  
01328 *        MOVE TRANSLATION TO TCAR                          *      ELGACL  
01329 *                                                          *      ELGACL  
01330 ************************************************************      ELGACL  
01331  0690-MOVE-TRANSLATION-TO-TCAR.                                   ELGACL  
01332 *    *-----------------------------------------------------------*ELGACL  
01333 *    *  PERFORMED BY 0520-GET-FYI-PHRASE,                        *ELGACL  
01334 *    *      0560-GET-COST-CONTAINMENT-PHRA,                      *ELGACL  
01335 *    *      0600-GET-CONDITION-BITS-PHRASE,                      *ELGACL  
01336 *    *      0620-GET-PLACE-OF-TREATMENT-PH,                      *ELGACL  
01337 *    *      0410-GET-L-O-B-PHRA.                                 *ELGACL  
01338 *    *-----------------------------------------------------------*ELGACL  
01339      PERFORM WITH TEST BEFORE                                     ELGACL  
01340          VARYING CMF-DESCR-IDX FROM +1 BY +1                      ELGACL  
01341               UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES           ELGACL  
01342            IF TCAR-FROM-SUB NOT < 20                              ELGACL  
01343               PERFORM 0700-SEND-PART-PARA THRU 0700-END           ELGACL  
01344            END-IF                                                 ELGACL  
01345                 ADD +1 TO  TCAR-FROM-SUB                          ELGACL  
01346                 MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)               ELGACL  
01347                     TO  TCAR-FROM-LINE (TCAR-FROM-SUB)            ELGACL  
01348      END-PERFORM.                                                 ELGACL  
01349    0690-END.                                                      ELGACL  
01350      EXIT.                                                        ELGACL  
01351      EJECT                                                        ELGACL  
01352                                                                   ELGACL  
01353                                                                   ELGACL  
01354 ************************************************************      ELGACL  
01355 *                                                          *      ELGACL  
01356 *        SEND PARTIAL PARAGRAPH                            *      ELGACL  
01357 *                                                          *      ELGACL  
01358 ************************************************************      ELGACL  
01359  0700-SEND-PART-PARA.                                             ELGACL  
01360 *    *-----------------------------------------------------------*ELGACL  
01361 *    *  PERFORMED BY 0670 MOVE TRANSLATION TO TCAR               *ELGACL  
01362 *    *-----------------------------------------------------------*ELGACL  
01363 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGACL  
01364      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGACL  
01365 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGACL  
01366      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGACL  
01367      MOVE +79                                                     ELGACL  
01368        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGACL  
01369           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGACL  
01370           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGACL  
01371           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGACL  
01372           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGACL  
01373           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGACL  
01374           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGACL  
01375           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGACL  
01376           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGACL  
01377           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGACL  
01378 * -- UNSTRING/FLOW THE OUTPUT                                     ELGACL  
01379      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGACL  
01380 * -- MOVE FORMATTED TEXT TO OUTPUT ** EXCEPT LAST LINE **         ELGACL  
01381      PERFORM WITH TEST BEFORE                                     ELGACL  
01382            VARYING TCAR-X FROM 1 BY 1                             ELGACL  
01383              UNTIL TCAR-X = TCAR-OUTPUT-FIELDS-USED               ELGACL  
01384 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGACL  
01385         IF COF-NBR-DTL-LINES NOT < 20                             ELGACL  
01386         THEN                                                      ELGACL  
01387            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGACL  
01388            INITIALIZE COF-DTL                                     ELGACL  
01389            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGACL  
01390         END-IF                                                    ELGACL  
01391 *    -- APPEND LINE TO OUTPUT                                     ELGACL  
01392         ADD 1 TO COF-NBR-DTL-LINES                                ELGACL  
01393         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGACL  
01394           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGACL  
01395      END-PERFORM.                                                 ELGACL  
01396 * -- PUT LAST LINE OF COMPRESSED/UNSTRUNG OUTPUT INTO FROM AREA   ELGACL  
01397      INITIALIZE TCAR-FROM-AREA                                    ELGACL  
01398                 TCAR-FROM-LENGTH                                  ELGACL  
01399                 TCAR-FROM-SUB.                                    ELGACL  
01400      MOVE 1 TO TCAR-FROM-SUB.                                     ELGACL  
01401      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELGACL  
01402        TO TCAR-FROM-LINE (1).                                     ELGACL  
01403    0700-END.                                                      ELGACL  
01404      EXIT.                                                        ELGACL  
01405      EJECT                                                        ELGACL  
01406                                                                   ELGACL  
01407                                                                   ELGACL  
01408 ************************************************************      ELGACL  
01409 *                                                          *      ELGACL  
01410 *        COMPLETE AND SEND PARAGRAPH                       *      ELGACL  
01411 *                                                          *      ELGACL  
01412 ************************************************************      ELGACL  
01413  0710-COMPLETE-AND-SEND-PARA.                                     ELGACL  
01414 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGACL  
01415      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGACL  
01416 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGACL  
01417      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGACL  
01418      MOVE +79                                                     ELGACL  
01419        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGACL  
01420           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGACL  
01421           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGACL  
01422           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGACL  
01423           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGACL  
01424           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGACL  
01425           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGACL  
01426           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGACL  
01427           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGACL  
01428           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGACL  
01429 * -- UNSTRING/FLOW THE OUTPUT                                     ELGACL  
01430      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGACL  
01431 * -- MOVE FORMATTED TEXT TO OUTPUT                                ELGACL  
01432      PERFORM WITH TEST BEFORE                                     ELGACL  
01433            VARYING TCAR-X FROM 1 BY 1                             ELGACL  
01434              UNTIL TCAR-X > TCAR-OUTPUT-FIELDS-USED               ELGACL  
01435 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGACL  
01436         IF COF-NBR-DTL-LINES NOT < 20                             ELGACL  
01437         THEN                                                      ELGACL  
01438            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGACL  
01439            INITIALIZE COF-DTL                                     ELGACL  
01440            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGACL  
01441         END-IF                                                    ELGACL  
01442 *    -- APPEND LINE TO OUTPUT                                     ELGACL  
01443         ADD 1 TO COF-NBR-DTL-LINES                                ELGACL  
01444         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGACL  
01445           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGACL  
01446      END-PERFORM.                                                 ELGACL  
01447                                                                   ELGACL  
01448 * -- CLEAR THE COMPRESSION WORK AREA                              ELGACL  
01449      INITIALIZE TCAR-FROM-AREA                                    ELGACL  
01450                 TCAR-FROM-SUB.                                    ELGACL  
01451    0710-END.                                                      ELGACL  
01452      EXIT.                                                        ELGACL  
01453      EJECT                                                        ELGACL  
01454                                                                   ELGACL  
01455                                                                   ELGACL  
01456                                                                   ELGACL  
01457 ************************************************************      ELGACL  
01458 *                                                          *      ELGACL  
01459 *        INSERT A BLANK LINE                               *      ELGACL  
01460 *                                                          *      ELGACL  
01461 ************************************************************      ELGACL  
01462  0720-INSERT-BLANK-LINE.                                          ELGACL  
01463      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGACL  
01464      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELGACL  
01465 * -- CALL THE OUTPUT MODULE                                       ELGACL  
01466      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA.                  ELGACL  
01467      INITIALIZE COF-DTL.                                          ELGACL  
01468      MOVE ZERO TO COF-NBR-DTL-LINES.                              ELGACL  
01469    0720-END.                                                      ELGACL  
01470      EXIT.                                                        ELGACL  
01471      EJECT                                                        ELGACL  
01472                                                                   ELGACL  
01473                                                                   ELGACL  
01474                                                                   ELGACL  
01475 ************************************************************      ELGACL  
01476 *                                                          *      ELGACL  
01477 *        INITIALIZE INTERNAL SWITCHES                      *      ELGACL  
01478 *                                                          *      ELGACL  
01479 ************************************************************      ELGACL  
01480  0730-INITIALIZE-SWITCHES.                                        ELGACL  
01481 *    *-----------------------------------------------------------*ELGACL  
01482 *    *  PERFORMED BY 0270-PROCESS-PERCENT-PHRASE.                *ELGACL  
01483 *    *-----------------------------------------------------------*ELGACL  
01484      SET WS-HAS-NO-IBGR                                           ELGACL  
01485          WS-HAS-NO-IDGD                                           ELGACL  
01486          WS-HAS-NO-IPGN                                           ELGACL  
01487          WS-HAS-NO-IPGP                                           ELGACL  
01488          WS-HAS-NO-IPGT                                           ELGACL  
01489          WS-HAS-NO-IPGS                                           ELGACL  
01490          WS-HAS-NO-INTERNAL TO TRUE.                              ELGACL  
01491    0730-END.                                                      ELGACL  
01492      EXIT.                                                        ELGACL  
01493      EJECT                                                        ELGACL  
01494                                                                   ELGACL  
01495                                                                   ELGACL  
01496 ************************************************************      ELGACL  
01497 *                                                          *      ELGACL  
01498 *        CHECK FOR INTERNALS                               *      ELGACL  
01499 *                                                          *      ELGACL  
01500 ************************************************************      ELGACL  
01501  0740-CHECK-FOR-INTERNALS.                                        ELGACL  
01502 *    *-----------------------------------------------------------*ELGACL  
01503 *    *  PERFORMED BY 0290-CREATE-SINGLE-PCENT-PHRA,              *ELGACL  
01504 *    *            0310-CREATE-PCENT-VALUE-PHRA.                  *ELGACL  
01505 *    *-----------------------------------------------------------*ELGACL  
01506                                                                   ELGACL  
01507      INITIALIZE WS-INTERNAL-TAB-SWITCHES.                         ELGACL  
01508                                                                   ELGACL  
01509      IF  NO-IBGR-SLOT-NBR (ASC-DES-INDEX)                         ELGACL  
01510          CONTINUE                                                 ELGACL  
01511      ELSE                                                         ELGACL  
01512          SET WS-HAS-IBGR                                          ELGACL  
01513              WS-HAS-AN-INTERNAL TO TRUE.                          ELGACL  
01514                                                                   ELGACL  
01515      IF  NO-IDGD-SLOT-NBR (ASC-DES-INDEX)                         ELGACL  
01516          CONTINUE                                                 ELGACL  
01517      ELSE                                                         ELGACL  
01518          SET WS-HAS-IDGD                                          ELGACL  
01519              WS-HAS-AN-INTERNAL TO TRUE.                          ELGACL  
01520                                                                   ELGACL  
01521      IF  NO-IPGN-SLOT-NBR (ASC-DES-INDEX)                         ELGACL  
01522          CONTINUE                                                 ELGACL  
01523      ELSE                                                         ELGACL  
01524          SET WS-HAS-IPGN                                          ELGACL  
01525              WS-HAS-AN-INTERNAL TO TRUE.                          ELGACL  
01526                                                                   ELGACL  
01527      IF  NO-IPGP-SLOT-NBR (ASC-DES-INDEX)                         ELGACL  
01528          CONTINUE                                                 ELGACL  
01529      ELSE                                                         ELGACL  
01530          SET WS-HAS-IPGP                                          ELGACL  
01531              WS-HAS-AN-INTERNAL TO TRUE.                          ELGACL  
01532                                                                   ELGACL  
01533      IF  NO-IPGT-SLOT-NBR (ASC-DES-INDEX)                         ELGACL  
01534          CONTINUE                                                 ELGACL  
01535      ELSE                                                         ELGACL  
01536          SET WS-HAS-IPGT                                          ELGACL  
01537              WS-HAS-AN-INTERNAL TO TRUE.                          ELGACL  
01538                                                                   ELGACL  
01539      IF  ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX) NOT NUMERIC          ELGACL  
01540        MOVE ZERO TO ACCUM-IPGS-SLOT-NBR (ASC-DES-INDEX)           ELGACL  
01541      END-IF.                                                      ELGACL  
01542                                                                   ELGACL  
01543      IF  NO-IPGS-SLOT-NBR (ASC-DES-INDEX)                         ELGACL  
01544          CONTINUE                                                 ELGACL  
01545      ELSE                                                         ELGACL  
01546          SET WS-HAS-IPGS                                          ELGACL  
01547              WS-HAS-AN-INTERNAL TO TRUE.                          ELGACL  
01548                                                                   ELGACL  
01549    0740-END.                                                      ELGACL  
01550      EXIT.                                                        ELGACL  
01551      EJECT                                                        ELGACL  
01552                                                                   ELGACL  
01553                                                                   ELGACL  
01554 ************************************************************      ELGACL  
01555 *                                                          *      ELGACL  
01556 *        CALL INTERNALS                                    *      ELGACL  
01557 *                                                          *      ELGACL  
01558 ************************************************************      ELGACL  
01559  0750-CALL-INTERNALS.                                             ELGACL  
01560 *    *-----------------------------------------------------------*ELGACL  
01561 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGACL  
01562 *    *-----------------------------------------------------------*ELGACL  
01563      IF WS-HAS-IBGR                                               ELGACL  
01564         MOVE PC-IBGR             TO  SRP-INTERNAL-TAB-ID          ELGACL  
01565         EXEC CICS LINK PROGRAM ('ELGIBGR')                        ELGACL  
01566                        COMMAREA (DFHCOMMAREA)                     ELGACL  
01567                        END-EXEC.                                  ELGACL  
01568                                                                   ELGACL  
01569      IF WS-HAS-IDGD                                               ELGACL  
01570         MOVE PC-IDGD             TO  SRP-INTERNAL-TAB-ID          ELGACL  
01571         EXEC CICS LINK PROGRAM ('ELGIDGD')                        ELGACL  
01572                        COMMAREA (DFHCOMMAREA)                     ELGACL  
01573                        END-EXEC.                                  ELGACL  
01574                                                                   ELGACL  
01575      IF WS-HAS-IPGP                                               ELGACL  
01576         MOVE PC-IPGP             TO  SRP-INTERNAL-TAB-ID          ELGACL  
01577         EXEC CICS LINK PROGRAM ('ELGIPGP')                        ELGACL  
01578                        COMMAREA (DFHCOMMAREA)                     ELGACL  
01579                        END-EXEC.                                  ELGACL  
01580                                                                   ELGACL  
01581      IF WS-HAS-IPGN                                               ELGACL  
01582         MOVE PC-IPGN             TO  SRP-INTERNAL-TAB-ID          ELGACL  
01583         EXEC CICS LINK PROGRAM ('ELGIPGN')                        ELGACL  
01584                        COMMAREA (DFHCOMMAREA)                     ELGACL  
01585                        END-EXEC.                                  ELGACL  
01586                                                                   ELGACL  
01587      IF WS-HAS-IPGT                                               ELGACL  
01588         MOVE PC-IPGT             TO  SRP-INTERNAL-TAB-ID          ELGACL  
01589         EXEC CICS LINK PROGRAM ('ELGIPGT')                        ELGACL  
01590                        COMMAREA (DFHCOMMAREA)                     ELGACL  
01591                        END-EXEC.                                  ELGACL  
01592                                                                   ELGACL  
01593      IF WS-HAS-IPGS                                               ELGACL  
01594         MOVE PC-IPGS             TO  SRP-INTERNAL-TAB-ID          ELGACL  
01595         EXEC CICS LINK PROGRAM ('ELGIPGS')                        ELGACL  
01596                        COMMAREA (DFHCOMMAREA)                     ELGACL  
01597                        END-EXEC.                                  ELGACL  
01598                                                                   ELGACL  
01599    0750-END.                                                      ELGACL  
01600      EXIT.                                                        ELGACL  
01601      EJECT                                                        ELGACL  
01602                                                                   ELGACL  
01603                                                                   ELGACL  
01604 ************************************************************      ELGACL  
01605 *                                                          *      ELGACL  
01606 *        GET INTERNAL TABULARS LIST                        *      ELGACL  
01607 *                                                          *      ELGACL  
01608 ************************************************************      ELGACL  
01609  0760-GET-INTERNAL-TABULARS-LIS.                                  ELGACL  
01610 *    *-----------------------------------------------------------*ELGACL  
01611 *    *  PERFORMED BY 0500-CREATE-COINSURANCE-APPLIC              *ELGACL  
01612 *    *-----------------------------------------------------------*ELGACL  
01613      INITIALIZE WS-INT-TAB-LST                                    ELGACL  
01614                 WS-INT-TAB-TXT                                    ELGACL  
01615                 WS-TAB-SUB.                                       ELGACL  
01616                                                                   ELGACL  
01617      IF WS-HAS-IBGR                                               ELGACL  
01618         ADD 1 TO WS-TAB-SUB                                       ELGACL  
01619         SET WS-INT-TAB-IBGR (WS-TAB-SUB)                          ELGACL  
01620             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACL  
01621                                                                   ELGACL  
01622      IF WS-HAS-IDGD                                               ELGACL  
01623         ADD 1 TO WS-TAB-SUB                                       ELGACL  
01624         SET WS-INT-TAB-IDGD (WS-TAB-SUB)                          ELGACL  
01625             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACL  
01626                                                                   ELGACL  
01627      IF WS-HAS-IPGN                                               ELGACL  
01628         ADD 1 TO WS-TAB-SUB                                       ELGACL  
01629         SET WS-INT-TAB-IPGN (WS-TAB-SUB)                          ELGACL  
01630             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACL  
01631                                                                   ELGACL  
01632      IF WS-HAS-IPGP                                               ELGACL  
01633         ADD 1 TO WS-TAB-SUB                                       ELGACL  
01634         SET WS-INT-TAB-IPGP (WS-TAB-SUB)                          ELGACL  
01635             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACL  
01636                                                                   ELGACL  
01637      IF WS-HAS-IPGT                                               ELGACL  
01638         ADD 1 TO WS-TAB-SUB                                       ELGACL  
01639         SET WS-INT-TAB-IPGT (WS-TAB-SUB)                          ELGACL  
01640             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACL  
01641                                                                   ELGACL  
01642      IF WS-HAS-IPGS                                               ELGACL  
01643         ADD 1 TO WS-TAB-SUB                                       ELGACL  
01644         SET WS-INT-TAB-IPGS (WS-TAB-SUB)                          ELGACL  
01645             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACL  
01646                                                                   ELGACL  
01647      IF WS-TAB-SUB  >  0                                          ELGACL  
01648         SET WS-INT-TAB-END (WS-TAB-SUB) TO TRUE                   ELGACL  
01649         IF WS-TAB-SUB  >  1                                       ELGACL  
01650            SET WS-INT-TAB-AND (WS-TAB-SUB - 1) TO TRUE            ELGACL  
01651         END-IF                                                    ELGACL  
01652         ADD  +1  TO  TCAR-FROM-SUB                                ELGACL  
01653         MOVE PC-SUBJECT-TO     TO  TCAR-FROM-LINE (TCAR-FROM-SUB) ELGACL  
01654                                                                   ELGACL  
01655         ADD  +1  TO  TCAR-FROM-SUB                                ELGACL  
01656         MOVE WS-INT-TAB-TXT-1  TO TCAR-FROM-LINE  (TCAR-FROM-SUB) ELGACL  
01657                                                                   ELGACL  
01658         IF WS-TAB-SUB > 3                                         ELGACL  
01659         THEN                                                      ELGACL  
01660            ADD 1 TO TCAR-FROM-SUB                                 ELGACL  
01661            MOVE WS-INT-TAB-TXT-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB)ELGACL  
01662         END-IF                                                    ELGACL  
01663         ADD 1 TO TCAR-FROM-SUB                                    ELGACL  
01664         MOVE PC-CONSIDERATIONS TO  TCAR-FROM-LINE (TCAR-FROM-SUB) ELGACL  
01665      END-IF.                                                      ELGACL  
01666    0760-END.                                                      ELGACL  
01667      EXIT.                                                        ELGACL  
01668      EJECT                                                        ELGACL  
01669                                                                   ELGACL  
01670 ************************************************************      ELGACL  
01671 *                                                          *      ELGACL  
01672 *        CREATE DEFINITION SENTENCE                        *      ELGACL  
01673 *                                                          *      ELGACL  
01674 ************************************************************      ELGACL  
01675  0800-CREATE-DEFINITION-SENTENC.                                  ELGACL  
01676 *    *-----------------------------------------------------------*ELGACL  
01677 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGACL  
01678 *    *-----------------------------------------------------------*ELGACL  
01679      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
01680      MOVE WS-DEFINITION-LINE                                      ELGACL  
01681           TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                     ELGACL  
01682      PERFORM 0810-TRANSLATE-DEFINITION-INDI THRU 0810-END.        ELGACL  
01683      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGACL  
01684      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
01685      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGACL  
01686      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGACL  
01687    0800-END.                                                      ELGACL  
01688      EXIT.                                                        ELGACL  
01689      EJECT                                                        ELGACL  
01690                                                                   ELGACL  
01691                                                                   ELGACL  
01692 ************************************************************      ELGACL  
01693 *                                                          *      ELGACL  
01694 *        TRANSLATE DEFINITION INDICATOR                    *      ELGACL  
01695 *                                                          *      ELGACL  
01696 ************************************************************      ELGACL  
01697  0810-TRANSLATE-DEFINITION-INDI.                                  ELGACL  
01698 *    *-----------------------------------------------------------*ELGACL  
01699 *    *  PERFORMED BY 0800-CREATE-DEFINITION-SENTENC.             *ELGACL  
01700 *    *-----------------------------------------------------------*ELGACL  
01701      MOVE ACCUM-DEFINITION   TO  CMF-CODE-VALUE.                  ELGACL  
01702      MOVE 'COINS-DEFINITION' TO  CMF-ELEMENT-SYSTEM-NAME.         ELGACL  
01703      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01704    0810-END.                                                      ELGACL  
01705      EXIT.                                                        ELGACL  
01706      EJECT                                                        ELGACL  
01707                                                                   ELGACL  
01708                                                                   ELGACL  
01709 ************************************************************      ELGACL  
01710 *                                                          *      ELGACL  
01711 *        CREATE BENEFIT PERIOD SENTENCE                    *      ELGACL  
01712 *                                                          *      ELGACL  
01713 ************************************************************      ELGACL  
01714  0830-CREATE-BENEFIT-PERIOD-SEN.                                  ELGACL  
01715 *    *-----------------------------------------------------------*ELGACL  
01716 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGACL  
01717 *    *-----------------------------------------------------------*ELGACL  
01718      IF TCAR-FROM-SUB > 1                                         ELGACL  
01719         PERFORM 0700-SEND-PART-PARA THRU 0700-END.                ELGACL  
01720      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
01721      MOVE WS-BEN-PER-PHRASE  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGACL  
01722      PERFORM 0850-TRANSLATE-BENEFIT-PERIOD THRU 0850-END.         ELGACL  
01723      PERFORM 0690-MOVE-TRANSLATION-TO-TCAR THRU 0690-END.         ELGACL  
01724      IF ACCUM-BEN-PER-TIME-FCTR NOT = ZEROS                       ELGACL  
01725          PERFORM 0860-GET-BP-TIME-QUAL-FCTR-PHR THRU 0860-END.    ELGACL  
01726      IF ACCUM-INTERVAL-TIME-FCTR NOT = ZEROS                      ELGACL  
01727          PERFORM 0880-GET-INTV-TIME-QUAL-FTR-PH THRU 0880-END.    ELGACL  
01728      IF ACCUM-INTERVAL-OVRD-IND NOT = ZEROS                       ELGACL  
01729          PERFORM 0900-CREATE-INTERVAL-OVERRIDE THRU 0900-END.     ELGACL  
01730      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
01731      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGACL  
01732      PERFORM 0710-COMPLETE-AND-SEND-PARA THRU 0710-END.           ELGACL  
01733    0830-END.                                                      ELGACL  
01734      EXIT.                                                        ELGACL  
01735      EJECT                                                        ELGACL  
01736                                                                   ELGACL  
01737                                                                   ELGACL  
01738 ************************************************************      ELGACL  
01739 *                                                          *      ELGACL  
01740 *        INITIALIZE COMPRESS AREA                          *      ELGACL  
01741 *                                                          *      ELGACL  
01742 ************************************************************      ELGACL  
01743  0840-INITIALIZE-COMPRESS-AREA.                                   ELGACL  
01744 *    *-----------------------------------------------------------*ELGACL  
01745 *    *  PERFORMED BY 0800-CREATE-DEFINITION-SENTENC              *ELGACL  
01746 *    *      0830-CREATE-BENEFIT-PERIOD-SEN.                      *ELGACL  
01747 *    *-----------------------------------------------------------*ELGACL  
01748      INITIALIZE TCAR-FROM-AREA                                    ELGACL  
01749                 TCAR-FROM-LENGTH                                  ELGACL  
01750                 TCAR-FROM-SUB.                                    ELGACL  
01751    0840-END.                                                      ELGACL  
01752      EXIT.                                                        ELGACL  
01753      EJECT                                                        ELGACL  
01754                                                                   ELGACL  
01755                                                                   ELGACL  
01756 ************************************************************      ELGACL  
01757 *                                                          *      ELGACL  
01758 *        TRANSLATE BENEFIT PERIOD                          *      ELGACL  
01759 *                                                          *      ELGACL  
01760 ************************************************************      ELGACL  
01761  0850-TRANSLATE-BENEFIT-PERIOD.                                   ELGACL  
01762 *    *-----------------------------------------------------------*ELGACL  
01763 *    *  PERFORMED BY 0830-CREATE-BENEFIT-PERIOD-SEN.             *ELGACL  
01764 *    *-----------------------------------------------------------*ELGACL  
01765      MOVE ACCUM-BENEFIT-PERIOD   TO  CMF-CODE-VALUE.              ELGACL  
01766      MOVE 'COINS-BENEFIT-PERIOD' TO  CMF-ELEMENT-SYSTEM-NAME.     ELGACL  
01767      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01768    0850-END.                                                      ELGACL  
01769      EXIT.                                                        ELGACL  
01770      EJECT                                                        ELGACL  
01771                                                                   ELGACL  
01772                                                                   ELGACL  
01773 ************************************************************      ELGACL  
01774 *                                                          *      ELGACL  
01775 *        GET BP TIME QUAL FCTR PHRASE                      *      ELGACL  
01776 *                                                          *      ELGACL  
01777 ************************************************************      ELGACL  
01778  0860-GET-BP-TIME-QUAL-FCTR-PHR.                                  ELGACL  
01779 *    *-----------------------------------------------------------*ELGACL  
01780 *    *  PERFORMED BY 0830-CREATE-BENEFIT-PERIOD-SEN.             *ELGACL  
01781 *    *-----------------------------------------------------------*ELGACL  
01782      MOVE ACCUM-BEN-PER-TIME-FCTR  TO  WS-BP-TIME-FCTR.           ELGACL  
01783      PERFORM 0870-TRANSLATE-BEN-PER-TIME-QU THRU 0870-END.        ELGACL  
01784      MOVE CMF-DESCR-LINE (1)  TO  WS-BP-TIME-QUAL.                ELGACL  
01785      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
01786      MOVE WS-BP-TIME-FCTR-QUAL-PHRASE TO                          ELGACL  
01787           TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACL  
01788    0860-END.                                                      ELGACL  
01789      EXIT.                                                        ELGACL  
01790                                                                   ELGACL  
01791                                                                   ELGACL  
01792 ************************************************************      ELGACL  
01793 *                                                          *      ELGACL  
01794 *        TRANSLATE BEN PER TIME QUALIFIER                  *      ELGACL  
01795 *                                                          *      ELGACL  
01796 ************************************************************      ELGACL  
01797  0870-TRANSLATE-BEN-PER-TIME-QU.                                  ELGACL  
01798 *    *-----------------------------------------------------------*ELGACL  
01799 *    *  PERFORMED BY 0860-GET-BP-TIME-QUAL-FCTR-PHR.             *ELGACL  
01800 *    *-----------------------------------------------------------*ELGACL  
01801      MOVE ACCUM-BEN-PER-TIME-QUAL   TO  CMF-CODE-VALUE.           ELGACL  
01802      MOVE 'COINS-BEN-PER-TIME-QUAL' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGACL  
01803      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01804    0870-END.                                                      ELGACL  
01805      EXIT.                                                        ELGACL  
01806                                                                   ELGACL  
01807                                                                   ELGACL  
01808 ************************************************************      ELGACL  
01809 *                                                          *      ELGACL  
01810 *        GET INTV TIME QUAL FTR PH                         *      ELGACL  
01811 *                                                          *      ELGACL  
01812 ************************************************************      ELGACL  
01813  0880-GET-INTV-TIME-QUAL-FTR-PH.                                  ELGACL  
01814 *    *-----------------------------------------------------------*ELGACL  
01815 *    *  PERFORMED BY 0830-CREATE-BENEFIT-PERIOD-SEN.             *ELGACL  
01816 *    *-----------------------------------------------------------*ELGACL  
01817      MOVE ACCUM-INTERVAL-TIME-FCTR  TO  WS-INTERVAL-TIME-FCTR.    ELGACL  
01818      PERFORM 0890-XLATE-INTERVAL-TIME-FCTR THRU 0890-END.         ELGACL  
01819      MOVE CMF-DESCR-LINE (1)  TO  WS-INTERVAL-TIME-QUAL.          ELGACL  
01820      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGACL  
01821      MOVE WS-INTERVAL-TIME-FCTR-QUAL-PHR                          ELGACL  
01822               TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                 ELGACL  
01823    0880-END.                                                      ELGACL  
01824      EXIT.                                                        ELGACL  
01825                                                                   ELGACL  
01826                                                                   ELGACL  
01827 ************************************************************      ELGACL  
01828 *                                                          *      ELGACL  
01829 *        TRANSLATE INTERVAL TIME FACTOR                    *      ELGACL  
01830 *                                                          *      ELGACL  
01831 ************************************************************      ELGACL  
01832  0890-XLATE-INTERVAL-TIME-FCTR.                                   ELGACL  
01833 *    *-----------------------------------------------------------*ELGACL  
01834 *    *  PERFORMED BY 0880-GET-INTV-TIME-QUAL-FTR-PH.             *ELGACL  
01835 *    *-----------------------------------------------------------*ELGACL  
01836      MOVE ACCUM-INTERVAL-TIME-FCTR  TO  CMF-CODE-VALUE.           ELGACL  
01837      MOVE 'COINS-INTERVAL-TIME-FCTR' TO  CMF-ELEMENT-SYSTEM-NAME. ELGACL  
01838      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01839    0890-END.                                                      ELGACL  
01840      EXIT.                                                        ELGACL  
01841                                                                   ELGACL  
01842                                                                   ELGACL  
01843 ************************************************************      ELGACL  
01844 *                                                          *      ELGACL  
01845 *        CREATE INTERVAL OVERRIDE NOTE                     *      ELGACL  
01846 *                                                          *      ELGACL  
01847 ************************************************************      ELGACL  
01848  0900-CREATE-INTERVAL-OVERRIDE.                                   ELGACL  
01849 *    *-----------------------------------------------------------*ELGACL  
01850 *    *  PERFORMED BY 0830-CREATE-BENEFIT-PERIOD-SEN.             *ELGACL  
01851 *    *-----------------------------------------------------------*ELGACL  
01852      ADD +1 TO TCAR-FROM-SUB.                                     ELGACL  
01853      MOVE PC-NOTE  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGACL  
01854      PERFORM 0910-CREATE-INTVL-OVERRIDE-PHR THRU 0910-END.        ELGACL  
01855      ADD +1 TO TCAR-FROM-SUB.                                     ELGACL  
01856      MOVE ')'      TO  TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGACL  
01857    0900-END.                                                      ELGACL  
01858      EXIT.                                                        ELGACL  
01859                                                                   ELGACL  
01860                                                                   ELGACL  
01861 ************************************************************      ELGACL  
01862 *                                                          *      ELGACL  
01863 *        CREATE INTERVAL OVERRIDE PHRASE                   *      ELGACL  
01864 *                                                          *      ELGACL  
01865 ************************************************************      ELGACL  
01866  0910-CREATE-INTVL-OVERRIDE-PHR.                                  ELGACL  
01867 *    *-----------------------------------------------------------*ELGACL  
01868 *    *  PERFORMED BY 0900-CREATE-INTERVAL-OVERRIDE               *ELGACL  
01869 *    *-----------------------------------------------------------*ELGACL  
01870      ADD +1 TO TCAR-FROM-SUB.                                     ELGACL  
01871      IF ACCUM-INTERVAL-OVRD-IND = '1'                             ELGACL  
01872         PERFORM 0911-CREATE-OVERRIDE-PHR-1 THRU 0911-END          ELGACL  
01873         ELSE IF ACCUM-INTERVAL-OVRD-IND = '2'                     ELGACL  
01874                 PERFORM 0912-CREATE-OVERRIDE-PHR-2 THRU 0912-END  ELGACL  
01875              ELSE IF ACCUM-INTERVAL-OVRD-IND = '3'                ELGACL  
01876                      PERFORM 0913-CREATE-OVERRIDE-PHR-3           ELGACL  
01877                         THRU 0913-END                             ELGACL  
01878                   ELSE PERFORM 0920-TRANSLATE-OVERRIDE-IND THRU   ELGACL  
01879                                0920-END.                          ELGACL  
01880    0910-END.                                                      ELGACL  
01881      EXIT.                                                        ELGACL  
01882                                                                   ELGACL  
01883                                                                   ELGACL  
01884 ************************************************************      ELGACL  
01885 *                                                          *      ELGACL  
01886 *        CREATE INTERVAL OVERRIDE PHRASE 1                 *      ELGACL  
01887 *                                                          *      ELGACL  
01888 ************************************************************      ELGACL  
01889 *    *-----------------------------------------------------------*ELGACL  
01890 *    *  PERFORMED BY 0910-CREATE-INTVL-OVERRIDE-PHR              *ELGACL  
01891 *    *-----------------------------------------------------------*ELGACL  
01892  0911-CREATE-OVERRIDE-PHR-1.                                      ELGACL  
01893      MOVE ACCUM-INTERVAL-OVRD-VALUE TO                            ELGACL  
01894           WS-INTERVAL-OVERRIDE-VALUE-1.                           ELGACL  
01895      MOVE WS-INTERVAL-OVERRIDE-PHRASE-1 TO                        ELGACL  
01896           TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACL  
01897    0911-END.                                                      ELGACL  
01898      EXIT.                                                        ELGACL  
01899                                                                   ELGACL  
01900                                                                   ELGACL  
01901 ************************************************************      ELGACL  
01902 *                                                          *      ELGACL  
01903 *        CREATE INTERVAL OVERRIDE PHRASE 2                 *      ELGACL  
01904 *                                                          *      ELGACL  
01905 ************************************************************      ELGACL  
01906 *    *-----------------------------------------------------------*ELGACL  
01907 *    *  PERFORMED BY 0910-CREATE-INTVL-OVERRIDE-PHR              *ELGACL  
01908 *    *-----------------------------------------------------------*ELGACL  
01909  0912-CREATE-OVERRIDE-PHR-2.                                      ELGACL  
01910      MOVE ACCUM-INTERVAL-OVRD-VALUE TO                            ELGACL  
01911           WS-INTERVAL-OVERRIDE-VALUE-2.                           ELGACL  
01912      MOVE WS-INTERVAL-OVERRIDE-PHRASE-2 TO                        ELGACL  
01913           TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACL  
01914    0912-END.                                                      ELGACL  
01915      EXIT.                                                        ELGACL  
01916                                                                   ELGACL  
01917                                                                   ELGACL  
01918 ************************************************************      ELGACL  
01919 *                                                          *      ELGACL  
01920 *        CREATE INTERVAL OVERRIDE PHRASE 3                 *      ELGACL  
01921 *                                                          *      ELGACL  
01922 ************************************************************      ELGACL  
01923 *    *-----------------------------------------------------------*ELGACL  
01924 *    *  PERFORMED BY 0910-CREATE-INTVL-OVERRIDE-PHR              *ELGACL  
01925 *    *-----------------------------------------------------------*ELGACL  
01926  0913-CREATE-OVERRIDE-PHR-3.                                      ELGACL  
01927      MOVE ACCUM-INTERVAL-OVRD-VALUE TO                            ELGACL  
01928           WS-INTERVAL-OVERRIDE-VALUE-3.                           ELGACL  
01929      MOVE WS-INTERVAL-OVERRIDE-PHRASE-3 TO                        ELGACL  
01930           TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACL  
01931    0913-END.                                                      ELGACL  
01932      EXIT.                                                        ELGACL  
01933                                                                   ELGACL  
01934                                                                   ELGACL  
01935 ************************************************************      ELGACL  
01936 *                                                          *      ELGACL  
01937 *        TRANSLATE OVERRIDE INDICATOR                      *      ELGACL  
01938 *                                                          *      ELGACL  
01939 ************************************************************      ELGACL  
01940 *    *-----------------------------------------------------------*ELGACL  
01941 *    *  PERFORMED BY 0910-CREATE-INTVL-OVERRIDE-PHR              *ELGACL  
01942 *    *-----------------------------------------------------------*ELGACL  
01943  0920-TRANSLATE-OVERRIDE-IND.                                     ELGACL  
01944      MOVE ACCUM-INTERVAL-OVRD-IND  TO  CMF-CODE-VALUE.            ELGACL  
01945      MOVE 'COINS-INTERVAL-OVRD-IND' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGACL  
01946      PERFORM 0930-LINK-TO-CODES-MANUAL-FORX THRU 0930-END.        ELGACL  
01947    0920-END.                                                      ELGACL  
01948      EXIT.                                                        ELGACL  
01949                                                                   ELGACL  
01950                                                                   ELGACL  
01951 ************************************************************      ELGACL  
01952 *                                                          *      ELGACL  
01953 *        LINK TO CODES MANUAL FOR TRANSLATION              *      ELGACL  
01954 *                                                          *      ELGACL  
01955 ************************************************************      ELGACL  
01956  0930-LINK-TO-CODES-MANUAL-FORX.                                  ELGACL  
01957 *    *-----------------------------------------------------------*ELGACL  
01958 *    *  PERFORMED BY 0530-TRANSLATE-FYI-VALUE,                   *ELGACL  
01959 *    *      0570-TRANSLATE-COST-CONTAINMEN,                      *ELGACL  
01960 *    *      0630-TRANSLATE-PLACE-OF-TREATM,                      *ELGACL  
01961 *    *      0680-TRANSLATE-RELATIONSHIP,                         *ELGACL  
01962 *    *      0650-GET-FROM-AGE-PHRASE,                            *ELGACL  
01963 *    *      0660-GET-TO-AGE-PHRASE,                               ELGACL  
01964 *    *      0400-TRANSLATE-FAMILY-OR-INDIV,                      *ELGACL  
01965 *    *      0420-TRANSLATE-LINE-OF-BUSINES,                      *ELGACL  
01966 *    *      0810-TRANSLATE-DEFINITION-INDI,                      *ELGACL  
01967 *    *      0850-TRANSLATE-BENEFIT-PERIOD,                       *ELGACL  
01968 *    *      0350-TRANSLATE-VALUE-QUALIFIER,                      *ELGACL  
01969 *    *      0870-TRANSLATE-BEN-PER-TIME-QU.                      *ELGACL  
01970 *    *-----------------------------------------------------------*ELGACL  
01971      MOVE PC-ACL  TO  CMF-RECORD-PREFIX.                          ELGACL  
01972      EXEC CICS LINK PROGRAM ('ELUCMIF')                           ELGACL  
01973                     COMMAREA (DFHCOMMAREA)                        ELGACL  
01974                     LENGTH (LENGTH OF DFHCOMMAREA)                ELGACL  
01975                     END-EXEC.                                     ELGACL  
01976      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGACL  
01977      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGACL  
01978              ADDRESS OF CMF-DESCR.                                ELGACL  
01979    0930-END.                                                      ELGACL  
01980      EXIT.                                                        ELGACL  
01981      EJECT                                                        ELGACL  
01982                                                                   ELGACL  
01983                                                                   ELGACL  
01984                                                                   ELGACL  
01985                                                                   ELGACL  
01986 ************************************************************      ELGACL  
01987 *                                                          *      ELGACL  
01988 *        DISPLAY COINSURANCE END SENTENCE                  *      ELGACL  
01989 *                                                          *      ELGACL  
01990 ************************************************************      ELGACL  
01991  0940-DISPLAY-COINSURANCE-END-S.                                  ELGACL  
01992 *    *-----------------------------------------------------------*ELGACL  
01993 *    *  PERFORMED BY 0260-DISPLAY-OCCURENCE-TEXT.                *ELGACL  
01994 *    *-----------------------------------------------------------*ELGACL  
01995      MOVE +2                  TO  COF-NBR-DTL-LINES.              ELGACL  
01996      MOVE COINSURANCE-END-MSG-1A TO COF-DTL-LINE (1).             ELGACL  
01997      MOVE COINSURANCE-END-MSG-1B TO COF-DTL-LINE (2).             ELGACL  
01998      PERFORM 1100-LINK-TO-OUTPUT-MODULE THRU 1100-END.            ELGACL  
01999    0940-END.                                                      ELGACL  
02000      EXIT.                                                        ELGACL  
02001                                                                   ELGACL  
02002                                                                   ELGACL  
02003 ************************************************************      ELGACL  
02004 *                                                          *      ELGACL  
02005 *        SETUP AND READ FILE                               *      ELGACL  
02006 *                                                          *      ELGACL  
02007 ************************************************************      ELGACL  
02008  0950-SETUP-AND-READ-FILE.                                        ELGACL  
02009 *    *-----------------------------------------------------------*ELGACL  
02010 *    *  PERFORMED BY 0250-DISPLAY-REGULAR-TEXT,                  *ELGACL  
02011 *    *      0260-DISPLAY-OCCURENCE-TEXT.                         *ELGACL  
02012 *    *-----------------------------------------------------------*ELGACL  
02013      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGACL  
02014      SET IOP-FCQ-NONE         TO  TRUE.                           ELGACL  
02015      SET IOP-KVQ-NONE         TO  TRUE.                           ELGACL  
02016      SET IOP-STG-MODE-LOCATE  TO  TRUE.                           ELGACL  
02017      PERFORM 0990-CALL-INPUT-OUTPUT-MODU THRU 0990-END.           ELGACL  
02018      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY                       ELGACL  
02019                               TO  IOP-REC-PTR.                    ELGACL  
02020    0950-END.                                                      ELGACL  
02021      EXIT.                                                        ELGACL  
02022      EJECT                                                        ELGACL  
02023                                                                   ELGACL  
02024                                                                   ELGACL  
02025 ************************************************************      ELGACL  
02026 *                                                          *      ELGACL  
02027 *        OBTAIN TOPIC HEADER                               *      ELGACL  
02028 *                                                          *      ELGACL  
02029 ************************************************************      ELGACL  
02030  0960-OBTAIN-TOPIC-HEADER.                                        ELGACL  
02031 *    *-----------------------------------------------------------*ELGACL  
02032 *    *  PERFORMED BY 0170-PROCESS, 0260-DISPLAY-OCCURENCE-TEXT.  *ELGACL  
02033 *    *-----------------------------------------------------------*ELGACL  
02034      MOVE +2            TO  COF-NBR-HDR-LINES.                    ELGACL  
02035      MOVE TOPIC-HEADER  TO  COF-HDR-LINE (2).                     ELGACL  
02036      MOVE  0            TO  COF-NBR-DTL-LINES.                    ELGACL  
02037      SET  COF-NEW-PAGE  TO  TRUE.                                 ELGACL  
02038      PERFORM 0970-SETUP-FOR-OUTPUT-LINK THRU 0970-END.            ELGACL  
02039    0960-END.                                                      ELGACL  
02040      EXIT.                                                        ELGACL  
02041                                                                   ELGACL  
02042                                                                   ELGACL  
02043 ************************************************************      ELGACL  
02044 *                                                          *      ELGACL  
02045 *        SETUP FOR OUTPUT LINK                             *      ELGACL  
02046 *                                                          *      ELGACL  
02047 ************************************************************      ELGACL  
02048  0970-SETUP-FOR-OUTPUT-LINK.                                      ELGACL  
02049 *    *-----------------------------------------------------------*ELGACL  
02050 *    *  PERFORMED BY 0200-PRINT-THE-SUB-HEADER,                  *ELGACL  
02051 *    *      0190-DISPLAY-NO-ACCUMS-MESSAGE,                      *ELGACL  
02052 *    *      0905-TEXT-COMPRESSION-PROCESS,                       *ELGACL  
02053 *    *      1240-PRINT-THE-HEADER.                               *ELGACL  
02054 *    *-----------------------------------------------------------*ELGACL  
02055      ADD  +1      TO  COF-NBR-DTL-LINES.                          ELGACL  
02056      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGACL  
02057      PERFORM 1100-LINK-TO-OUTPUT-MODULE THRU 1100-END.            ELGACL  
02058      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGACL  
02059    0970-END.                                                      ELGACL  
02060      EXIT.                                                        ELGACL  
02061      EJECT                                                        ELGACL  
02062                                                                   ELGACL  
02063                                                                   ELGACL  
02064 ************************************************************      ELGACL  
02065 *                                                          *      ELGACL  
02066 *        DELETE ACCUM OCCURENCE FILE                       *      ELGACL  
02067 *                                                          *      ELGACL  
02068 ************************************************************      ELGACL  
02069  0980-DELETE-ACCUM-OCCURENCE-FI.                                  ELGACL  
02070 *    *-----------------------------------------------------------*ELGACL  
02071 *    *  PERFORMED BY 0250-DISPLAY-REGULAR-TEXT.                  *ELGACL  
02072 *    *-----------------------------------------------------------*ELGACL  
02073      SET CIA-ELSWKFL1-DDN   TO TRUE.                              ELGACL  
02074      SET  IOP-DEL           TO  TRUE.                             ELGACL  
02075      SET  IOP-FCQ-NONE      TO  TRUE.                             ELGACL  
02076      SET  IOP-KVQ-NONE      TO  TRUE.                             ELGACL  
02077      PERFORM 0990-CALL-INPUT-OUTPUT-MODU THRU 0990-END.           ELGACL  
02078    0980-END.                                                      ELGACL  
02079      EXIT.                                                        ELGACL  
02080      EJECT                                                        ELGACL  
02081                                                                   ELGACL  
02082                                                                   ELGACL  
02083 ************************************************************      ELGACL  
02084 *                                                          *      ELGACL  
02085 *        LINK TO INPUT OUTPUT MODULE                       *      ELGACL  
02086 *                                                          *      ELGACL  
02087 ************************************************************      ELGACL  
02088  0990-CALL-INPUT-OUTPUT-MODU.                                     ELGACL  
02089 *    *-----------------------------------------------------------*ELGACL  
02090 *    *  PERFORMED BY 0980-DELETE-ACCUM-OCCURENCE-FI,             *ELGACL  
02091 *    *      0950-SETUP-AND-READ-FILE.                            *ELGACL  
02092 *    *-----------------------------------------------------------*ELGACL  
02093      CALL 'ELUIOPGM' USING DFHEIBLK                               ELGACL  
02094                            DFHCOMMAREA                            ELGACL  
02095                            ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS ELGACL  
02096                     END-CALL.                                     ELGACL  
02097    0990-END.                                                      ELGACL  
02098      EXIT.                                                        ELGACL  
02099      EJECT                                                        ELGACL  
02100                                                                   ELGACL  
02101                                                                   ELGACL  
02102 ************************************************************      ELGACL  
02103 *                                                          *      ELGACL  
02104 *        END THE DISPLAY                                   *      ELGACL  
02105 *                                                          *      ELGACL  
02106 ************************************************************      ELGACL  
02107  1000-END-THE-DISPLAY.                                            ELGACL  
02108 *    *-----------------------------------------------------------*ELGACL  
02109 *    *  PERFORMED BY 0170-PROCESS.                               *ELGACL  
02110 *    *-----------------------------------------------------------*ELGACL  
02111      IF COF-NBR-DTL-LINES > 0                                     ELGACL  
02112         PERFORM 1100-LINK-TO-OUTPUT-MODULE THRU 1100-END.         ELGACL  
02113      SET COF-END  TO  TRUE.                                       ELGACL  
02114      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELGACL  
02115      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGACL  
02116      PERFORM 1100-LINK-TO-OUTPUT-MODULE THRU 1100-END.            ELGACL  
02117    1000-END.                                                      ELGACL  
02118      EXIT.                                                        ELGACL  
02119                                                                   ELGACL  
02120                                                                   ELGACL  
02121 ************************************************************      ELGACL  
02122 *                                                          *      ELGACL  
02123 *        LINK TO OUTPUT MODULE                             *      ELGACL  
02124 *                                                          *      ELGACL  
02125 ************************************************************      ELGACL  
02126  1100-LINK-TO-OUTPUT-MODULE.                                      ELGACL  
02127 *    *-----------------------------------------------------------*ELGACL  
02128 *    *  PERFORMED BY 0200-DISPLAY-NOT-APPLICABLE-LO,             *ELGACL  
02129 *    *      0300-CREATE-MULTI-PCENT-PHRA,                        *ELGACL  
02130 *    *      0310-CREATE-PCENT-VALUE-PHRA,                        *ELGACL  
02131 *    *      1000-END-THE-DISPLAY,                                *ELGACL  
02132 *    *      0940-DISPLAY-COINSURANCE-END-S,                      *ELGACL  
02133 *    *      0970-SETUP-FOR-OUTPUT-LINK.                          *ELGACL  
02134 *    *-----------------------------------------------------------*ELGACL  
02135      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELGACL  
02136                     COMMAREA (DFHCOMMAREA)                        ELGACL  
02137                     LENGTH (LENGTH OF DFHCOMMAREA)                ELGACL  
02138                     END-EXEC.                                     ELGACL  
02139    1100-END.                                                      ELGACL  
02140      EXIT.                                                        ELGACL  
