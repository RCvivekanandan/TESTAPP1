00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELPPRSSB
00003  PROGRAM-ID.         ELPPRSSB.                                       LV001
00004                                                                   ELPPRSSB
00005  AUTHOR.             EDWARD G LISS.                               ELPPRSSB
00006                                                                   ELPPRSSB
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELPPRSSB
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELPPRSSB
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELPPRSSB
00010                      233 N. MICHIGAN AVE                          ELPPRSSB
00011                      CHICAGO, ILLINOIS 60601                      ELPPRSSB
00012                                                                   ELPPRSSB
00013  DATE-WRITTEN.       12-JUN-1989.                                 ELPPRSSB
00014                                                                   ELPPRSSB
00015  DATE-COMPILED.                                                   ELPPRSSB
00016                                                                   ELPPRSSB
00017  SECURITY.           COPYRIGHT 1989,                              ELPPRSSB
00018                      HEALTH CARE SERVICE CORPORATION              ELPPRSSB
00019      SKIP3                                                        ELPPRSSB
00020  ENVIRONMENT DIVISION.                                            ELPPRSSB
00021                                                                   ELPPRSSB
00022  CONFIGURATION SECTION.                                           ELPPRSSB
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELPPRSSB
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELPPRSSB
00025     SKIP3                                                         ELPPRSSB
00026 ******************************************************************ELPPRSSB
00027 *                                                                *ELPPRSSB
00028 *                    ELS ABEND PROCESSING                        *ELPPRSSB
00029 *                                                                *ELPPRSSB
00030 *   THIS SUBROUTINE PRINTS THE SSCB CONTROL BLOCK FOR THE ELS    *ELPPRSSB
00031 *   ABEND PROCESSING.  THE ENTIRE SSCB IS PRINTED BY THIS        *ELPPRSSB
00032 *   MODULE.                                                      *ELPPRSSB
00033 *                                                                *ELPPRSSB
00034 ******************************************************************ELPPRSSB
00035 *                                                                *ELPPRSSB
00036 *                      MAINTENANCE HISTORY                       *ELPPRSSB
00037 *                                                                *ELPPRSSB
00038 *  MOD     DATE     BY  DRPT                ACTION               *ELPPRSSB
00039 * ----- ----------- --- ----- ---------------------------------- *ELPPRSSB
00040 * 01.00 12-JUN-1989 EGL       CREATED.                           *ELPPRSSB
00041 *                                                                *ELPPRSSB
00042 * 01.01 15-NOV-1989 AKK       MAKE CHANGES TO ABEND CODE DESCRIP-*ELPPRSSB
00043 *                             TIONS.                             *ELPPRSSB
00044 *                                                                *ELPPRSSB
00045 * 01.02 03-JAN-1990 EGL       CHANGED DETAIL LINE 18 TO 132 CHAR-*ELPPRSSB
00046 *                             ACTERS TO AVOID LOSING LAST CHAR.  *ELPPRSSB
00047 * 01.03 29-JAN-1990 EGL       ADDED ABEND CODES EL24 AND EL25    *ELPPRSSB
00048 *                             TO THE MESSAGE TABLE.              *ELPPRSSB
00049 *                                                                *ELPPRSSB
00050 * 01.04 01-OCT-1991 JPB       CHANGED SS-FAM-RELATIONSHIP FROM  -*ELPPRSSB
00051 *                             PIC X(01) TO PIC X(02 FOR THE      *ELPPRSSB
00052 *                             EXPANSION OF THE FAMILY RELATION-  *ELPPRSSB
00053 *                             SHIP INDICATOR.                    *ELPPRSSB
00054 *                                                                *ELPPRSSB
00055 * 01.04 03-NOV-1997 AKK       ADDED SUPPORT FOR YEAR 2000 AND    *ELPPRSSB
00056 *                             TX MERGE.                          *ELPPRSSB
00057 *                                                                *ELPPRSSB
00058 *                                                                *ELPPRSSB
00059 ******************************************************************ELPPRSSB
00060  TITLE 'ELS ABEND PROCESSING - PRINT SSCB SUBROUTINE'.            ELPPRSSB
00061  DATA DIVISION.                                                   ELPPRSSB
00062  WORKING-STORAGE SECTION.                                         ELPPRSSB
00063  01  FILLER           PIC X(18)   VALUE '*START OF ELPPRSSB'.     ELPPRSSB
00064 *                                                                 ELPPRSSB
00065  01  WS-MISC-STUFF.                                               ELPPRSSB
00066      05  WS-WORK-DATE.                                            ELPPRSSB
00067          10  WS-WORK-CEN           PIC 9(02).                     ELPPRSSB
00068          10  WS-WORK-JUL           PIC 9(03).                     ELPPRSSB
00069                                                                   ELPPRSSB
00070      05  WS-JULIAN-DATE            PIC 9(5).                      ELPPRSSB
00071      05  WS-GREGORIAN-DATE         PIC 9(6).                      ELPPRSSB
00072      05  WS-INVALID-DATA-SW        PIC X.                         ELPPRSSB
00073          88  WS-NO-INVALID-DATA-FOUND         VALUE 'N'.          ELPPRSSB
00074          88  WS-INVALID-DATA-FOUND            VALUE 'Y'.          ELPPRSSB
00075      05  WS-SUB                    PIC S9(4)  COMP SYNC.          ELPPRSSB
00076      05  WS-CONTRACT-TABLE.                                       ELPPRSSB
00077          10  FILLER                PIC X(10) VALUE 'INST BASIC'.  ELPPRSSB
00078          10  FILLER                PIC X(10) VALUE 'INST SUPPL'.  ELPPRSSB
00079          10  FILLER                PIC X(10) VALUE 'PROF BASIC'.  ELPPRSSB
00080          10  FILLER                PIC X(10) VALUE 'PROF SUPPL'.  ELPPRSSB
00081      05  WS-CONTRACT-TYPE    REDEFINES WS-CONTRACT-TABLE          ELPPRSSB
00082                              OCCURS 4 TIMES                       ELPPRSSB
00083                                    PIC X(10).                     ELPPRSSB
00084 /                                                                 ELPPRSSB
00085  01  WS-ABEND-MSG-TABLE.                                          ELPPRSSB
00086      05  WS-ABEND-MSGS.                                           ELPPRSSB
00087          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00088              'EL00UNDEFINED CONDITION OCCURRED                  '.ELPPRSSB
00089          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00090              'EL01MISSING/INCORRECT COMMAREA                    '.ELPPRSSB
00091          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00092              'EL02MISSING POINTER                               '.ELPPRSSB
00093          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00094              'EL03AREANAME NOT DEFINED IN SMA                   '.ELPPRSSB
00095          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00096              'EL04DDNAME NOT DEFINED IN SMA                     '.ELPPRSSB
00097          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00098              'EL05IOP POINTER NOT SET                           '.ELPPRSSB
00099          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00100              'EL06INVALID REQUEST TO ELUSTGMG                   '.ELPPRSSB
00101          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00102              'EL07INVALID REQUEST TO ELUIOPGM                   '.ELPPRSSB
00103          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00104              'EL08RECORD POINTER FOR WRITE IS NULL              '.ELPPRSSB
00105          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00106              'EL09FILE NOT OPEN                                 '.ELPPRSSB
00107          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00108              'EL10CRITICAL I/O ERROR                            '.ELPPRSSB
00109          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00110              'EL11BMS MAP FAIL ERROR                            '.ELPPRSSB
00111          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00112              'EL12NULL POINTER FOUND                            '.ELPPRSSB
00113          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00114              'EL20A CODING ERROR, POSSIBLE OVERLAPPING DATES    '.ELPPRSSB
00115          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00116              'EL21ERROR IN TOPIC SELECTOR TABLES                '.ELPPRSSB
00117          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00118              'EL22ERROR IN TOPIC SELECTOR TABLES                '.ELPPRSSB
00119          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00120              'EL23EXPECTED PARAMETER POINTER IS NULL            '.ELPPRSSB
00121          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00122              'EL24GROUP SPECIFIC RECORD IS UNAVAILABLE TO ELS   '.ELPPRSSB
00123          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00124              'EL25CONTRACT RECORD IS UNAVAILABLE TO ELS         '.ELPPRSSB
00125          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00126              'EL31SYSTEM NAME CANNOT BE FOUND IN CODES MANUAL   '.ELPPRSSB
00127          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00128              'EL32CODE VALUE NOT FOUND IN CODES MANUAL          '.ELPPRSSB
00129          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00130              'EL33DATA ELEMENT NOT FOUND IN CODES MANAUL        '.ELPPRSSB
00131          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00132              'EL34ENGLISH NAME NOT FOUND IN CODES MANUAL        '.ELPPRSSB
00133          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00134              'EL35RECORD CANNOT BE FOUND IN CODES MANUAL        '.ELPPRSSB
00135          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00136              'EL41DATES FILE RECORD NOT FOUND                   '.ELPPRSSB
00137          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00138              'EL42GROUP SPECIFIC RECORD NOT FOUND               '.ELPPRSSB
00139          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00140              'EL43CONTRACT RECORD NOT FOUND                     '.ELPPRSSB
00141          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00142              'EL44BENEFIT PROVISION RECORD NOT FOUND            '.ELPPRSSB
00143          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00144              'EL45TABULAR RECORD CANNOT BE FOUND                '.ELPPRSSB
00145          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00146              'EL46GCSYSTBL RECORD CANNOT BE FOUND               '.ELPPRSSB
00147          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00148              'EL47NO MATCH FOUND IN FIELD VALIDATION TABLE1     '.ELPPRSSB
00149          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00150              'EL48NO MATCH FOUND IN FIELD VALIDATION TABLE2     '.ELPPRSSB
00151          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00152              'EL50TABULAR ID UNDEFINED                          '.ELPPRSSB
00153          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00154              'EL51INVALID ASCEND/DESCEND INDICATOR              '.ELPPRSSB
00155          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00156              'EL52NO MATCHING ASCEND/DESCEND FOUND              '.ELPPRSSB
00157          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00158              'EL53CONTRACT FOUND/GROUP SPECIFIC NOT FOUND       '.ELPPRSSB
00159          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00160              'EL54GROUP SPECIFIC FOUND/CONTRACT NOT FOUND       '.ELPPRSSB
00161          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00162              'EL55COST CONTAINMENT CODED ON CONTRACT            '.ELPPRSSB
00163          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00164              'EL60INTERNAL TABLE OVERFLOW                       '.ELPPRSSB
00165          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00166              'EL61UNABLE TO FIND TABLE ARGUMENT                 '.ELPPRSSB
00167          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00168              'EL62UNABLE TO FIND GMF FILE                       '.ELPPRSSB
00169          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00170              'EL98INVALID/INSUFFICIENT PARMS                    '.ELPPRSSB
00171          10  FILLER              PIC X(54) VALUE                  ELPPRSSB
00172              'EL99INTERNAL PROGRAM LOGIC ERROR                  '.ELPPRSSB
00173      05  WS-ABEND-MSG-REDEFINES  REDEFINES WS-ABEND-MSGS          ELPPRSSB
00174                                  OCCURS 43 TIMES                  ELPPRSSB
00175                                  ASCENDING KEY WS-ABEND-CODE      ELPPRSSB
00176                                  INDEXED BY WS-ABEND-IDX.         ELPPRSSB
00177          10  WS-ABEND-CODE       PIC X(4).                        ELPPRSSB
00178          10  WS-ABEND-DESC       PIC X(50).                       ELPPRSSB
00179 /                                                                 ELPPRSSB
00180  01  SS-OUTPUT-LINES.                                             ELPPRSSB
00181      03  SS-DETAIL-LINE1A.                                        ELPPRSSB
00182          05  SS-DETAIL-LINE1A-CC   PIC X(01)  VALUE '1'.          ELPPRSSB
00183          05  FILLER                PIC X(24)  VALUE               ELPPRSSB
00184              'ABEND CODE DESCRIPTION: '.                          ELPPRSSB
00185          05  SS-ABEND-DESC         PIC X(108) VALUE SPACES.       ELPPRSSB
00186      03  SS-DETAIL-LINE1B.                                        ELPPRSSB
00187          05  SS-DETAIL-LINE1B-CC   PIC X(01)  VALUE '0'.          ELPPRSSB
00188          05  FILLER                PIC X(16)  VALUE               ELPPRSSB
00189              'SELECTION STATUS'.                                  ELPPRSSB
00190          05  FILLER                PIC X(116) VALUE SPACES.       ELPPRSSB
00191      03  SS-DETAIL-LINE2.                                         ELPPRSSB
00192          05  SS-DETAIL-LINE2-CC    PIC X(01)  VALUE ' '.          ELPPRSSB
00193          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00194          05  FILLER                PIC X(15)  VALUE               ELPPRSSB
00195              'SELECTOR STATE '.                                   ELPPRSSB
00196          05  SS-SELECTOR-STATE     PIC 9(04)  VALUE ZEROES.       ELPPRSSB
00197          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00198          05  FILLER                PIC X(12)  VALUE               ELPPRSSB
00199              'STATUS AREA '.                                      ELPPRSSB
00200          05  SS-STATUS-AREA        PIC X(25)  VALUE SPACES.       ELPPRSSB
00201          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRSSB
00202          05  FILLER                PIC X(07)  VALUE 'MODULE '.    ELPPRSSB
00203          05  SS-MODULE             PIC X(08)  VALUE SPACES.       ELPPRSSB
00204          05  FILLER                PIC X(53)  VALUE SPACES.       ELPPRSSB
00205      03  SS-DETAIL-LINE3.                                         ELPPRSSB
00206          05  SS-DETAIL-LINE3-CC    PIC X(01)  VALUE '0'.          ELPPRSSB
00207          05  FILLER                PIC X(18)  VALUE               ELPPRSSB
00208              'SELECTION CRITERIA'.                                ELPPRSSB
00209          05  FILLER                PIC X(14)  VALUE SPACES.       ELPPRSSB
00210          05  FILLER                PIC X(18)  VALUE               ELPPRSSB
00211              'ALL COVERAGE FROM '.                                ELPPRSSB
00212          05  SS-COV-FROM-DATE      PIC 99/99/99.                  ELPPRSSB
00213          05  SS-COV-FROM-DATE-A REDEFINES SS-COV-FROM-DATE        ELPPRSSB
00214                                    PIC X(08).                     ELPPRSSB
00215          05  FILLER                PIC X(04)  VALUE ' TO '.       ELPPRSSB
00216          05  SS-COV-TO-DATE        PIC 99/99/99.                  ELPPRSSB
00217          05  SS-COV-TO-DATE-A REDEFINES SS-COV-TO-DATE            ELPPRSSB
00218                                    PIC X(08).                     ELPPRSSB
00219          05  FILLER                PIC X(62)  VALUE SPACES.       ELPPRSSB
00220      03  SS-DETAIL-LINE4.                                         ELPPRSSB
00221          05  SS-DETAIL-LINE4-CC    PIC X(01)  VALUE '0'.          ELPPRSSB
00222          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00223          05  FILLER                PIC X(09)  VALUE 'PLAN CODE'.  ELPPRSSB
00224          05  FILLER                PIC X      VALUE SPACE.        ELPPRSSB
00225          05  SS-PLAN-CODE-LINE4    PIC X(03)  VALUE SPACE.        ELPPRSSB
00226          05  FILLER                PIC X      VALUE SPACE.        ELPPRSSB
00227          05  FILLER                PIC X(09)  VALUE               ELPPRSSB
00228              'GROUP NUM'.                                         ELPPRSSB
00229          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00230          05  SS-GROUP-NO-LINE4     PIC X(09)  VALUE SPACES.       ELPPRSSB
00231          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00232          05  FILLER                PIC X(05)  VALUE 'SECT '.      ELPPRSSB
00233          05  SS-SECTION-NO-LINE4   PIC X(05)  VALUE SPACES.       ELPPRSSB
00234          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00235          05  FILLER                PIC X(08)  VALUE 'PKG CODE'.   ELPPRSSB
00236          05  FILLER                PIC X      VALUE SPACES.       ELPPRSSB
00237          05  SS-PKG-CODE-NO-LINE4  PIC X(03)  VALUE SPACES.       ELPPRSSB
00238          05  FILLER                PIC X      VALUE SPACE.        ELPPRSSB
00239          05  FILLER                PIC X(10)  VALUE               ELPPRSSB
00240              'SERV FROM '.                                        ELPPRSSB
00241          05  SS-SERVICE-FROM-DATE  PIC 99/99/99.                  ELPPRSSB
00242          05  SS-SERVICE-FROM-DATE-A REDEFINES                     ELPPRSSB
00243               SS-SERVICE-FROM-DATE PIC X(08).                     ELPPRSSB
00244          05  FILLER                PIC X(04)  VALUE ' TO '.       ELPPRSSB
00245          05  SS-SERVICE-TO-DATE    PIC 99/99/99.                  ELPPRSSB
00246          05  SS-SERVICE-TO-DATE-A  REDEFINES SS-SERVICE-TO-DATE   ELPPRSSB
00247                                    PIC X(08).                     ELPPRSSB
00248          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRSSB
00249          05  FILLER                PIC X(11)  VALUE               ELPPRSSB
00250              'PROV CLASS '.                                       ELPPRSSB
00251          05  SS-PROVIDER-CLASS     PIC X(04)  VALUE SPACES.       ELPPRSSB
00252          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRSSB
00253          05  FILLER                PIC X(11)  VALUE               ELPPRSSB
00254              'SERV CLASS '.                                       ELPPRSSB
00255          05  SS-SERVICE-CLASS      PIC X(04)  VALUE SPACES.       ELPPRSSB
00256 *        05  FILLER                PIC X(09)  VALUE SPACES.       ELPPRSSB
00257      03  SS-DETAIL-LINE5.                                         ELPPRSSB
00258          05  SS-DETAIL-LINE5-CC    PIC X(01)  VALUE ' '.          ELPPRSSB
00259          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00260          05  FILLER                PIC X(09)  VALUE 'MEDICARE '.  ELPPRSSB
00261          05  SS-MEDICARE           PIC X(03)  VALUE SPACES.       ELPPRSSB
00262          05  FILLER                PIC X(10)  VALUE SPACES.       ELPPRSSB
00263          05  FILLER                PIC X(12)  VALUE               ELPPRSSB
00264              'PATIENT AGE '.                                      ELPPRSSB
00265          05  SS-PATIENT-AGE        PIC X(01)  VALUE SPACES.       ELPPRSSB
00266          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00267          05  FILLER                PIC X(20)  VALUE               ELPPRSSB
00268              'FAMILY RELATIONSHIP '.                              ELPPRSSB
00269          05  SS-FAM-REL            PIC X(01)  VALUE SPACES.       ELPPRSSB
00270          05  FILLER                PIC X(70)  VALUE SPACES.       ELPPRSSB
00271      03  SS-DETAIL-LINE6.                                         ELPPRSSB
00272          05  SS-DETAIL-LINE6-CC    PIC X(01)  VALUE ' '.          ELPPRSSB
00273          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00274          05  FILLER                PIC X(11)  VALUE 'SUBSCRIBER '.ELPPRSSB
00275          05  SS-SUBSCRIBER         PIC X(12)  VALUE SPACES.       ELPPRSSB
00276          05  FILLER                PIC X(15)  VALUE SPACES.       ELPPRSSB
00277          05  FILLER                PIC X(10)  VALUE 'EFFECTIVE '. ELPPRSSB
00278          05  SS-EFFECTIVE-DATE     PIC 99/99/99.                  ELPPRSSB
00279          05  SS-EFFECTIVE-DATE-A REDEFINES SS-EFFECTIVE-DATE      ELPPRSSB
00280                                    PIC X(08).                     ELPPRSSB
00281          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRSSB
00282          05  FILLER                PIC X(11)  VALUE 'TERMINATES '.ELPPRSSB
00283          05  SS-TERMINATE-DATE     PIC 99/99/99.                  ELPPRSSB
00284          05  SS-TERMINATE-DATE-A REDEFINES SS-TERMINATE-DATE      ELPPRSSB
00285                                    PIC X(08).                     ELPPRSSB
00286          05  FILLER                PIC X(50)  VALUE SPACES.       ELPPRSSB
00287      03  SS-DETAIL-LINE7.                                         ELPPRSSB
00288          05  SS-DETAIL-LINE7-CC    PIC X(01)  VALUE '0'.          ELPPRSSB
00289          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00290          05  FILLER                PIC X(05)  VALUE 'TOPIC'.      ELPPRSSB
00291          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRSSB
00292          05  SS-TOPIC              PIC X(16)  VALUE SPACES.       ELPPRSSB
00293          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRSSB
00294          05  FILLER                PIC X(12)  VALUE               ELPPRSSB
00295              'TOPIC PHRASE'.                                      ELPPRSSB
00296          05  FILLER                PIC X(05)  VALUE SPACES.       ELPPRSSB
00297          05  SS-TOPIC-PHRASE       PIC X(78)  VALUE SPACES.       ELPPRSSB
00298          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00299      03  SS-DETAIL-LINE8.                                         ELPPRSSB
00300          05  SS-DETAIL-LINE8-CC    PIC X(01)  VALUE ' '.          ELPPRSSB
00301          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00302          05  FILLER                PIC X(09)  VALUE 'SUB-TOPIC'.  ELPPRSSB
00303          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRSSB
00304          05  SS-SUB-TOPIC          PIC X(16)  VALUE SPACES.       ELPPRSSB
00305          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRSSB
00306          05  FILLER                PIC X(17)  VALUE               ELPPRSSB
00307              'SUB-TOPIC PHRASE '.                                 ELPPRSSB
00308          05  SS-SUB-TOPIC-PHRASE   PIC X(78)  VALUE SPACES.       ELPPRSSB
00309          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00310      03  SS-DETAIL-LINE9.                                         ELPPRSSB
00311          05  SS-DETAIL-LINE9-CC    PIC X(01)  VALUE ' '.          ELPPRSSB
00312          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00313          05  FILLER                PIC X(11)  VALUE 'MODIFIER 1 '.ELPPRSSB
00314          05  SS-MODIFIER-1         PIC X(16)  VALUE SPACES.       ELPPRSSB
00315          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRSSB
00316          05  FILLER                PIC X(08)  VALUE 'MODIFIER'.   ELPPRSSB
00317          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRSSB
00318          05  FILLER                PIC X(07)  VALUE 'PHRASE '.    ELPPRSSB
00319          05  SS-MODIFIER-PHRASE1   PIC X(78)  VALUE SPACES.       ELPPRSSB
00320          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00321      03  SS-DETAIL-LINE10.                                        ELPPRSSB
00322          05  SS-DETAIL-LINE10-CC   PIC X(01)  VALUE ' '.          ELPPRSSB
00323          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00324          05  FILLER                PIC X(11)  VALUE 'MODIFIER 2 '.ELPPRSSB
00325          05  SS-MODIFIER-2         PIC X(16)  VALUE SPACES.       ELPPRSSB
00326          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRSSB
00327          05  FILLER                PIC X(08)  VALUE 'MODIFIER'.   ELPPRSSB
00328          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRSSB
00329          05  FILLER                PIC X(07)  VALUE 'PHRASE '.    ELPPRSSB
00330          05  SS-MODIFIER-PHRASE2   PIC X(78)  VALUE SPACES.       ELPPRSSB
00331          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00332      03  SS-DETAIL-LINE11.                                        ELPPRSSB
00333          05  SS-DETAIL-LINE11-CC   PIC X(01)  VALUE '0'.          ELPPRSSB
00334          05  FILLER                PIC X(17)  VALUE               ELPPRSSB
00335              'COMMAND LINE DATA'.                                 ELPPRSSB
00336          05  FILLER                PIC X(115) VALUE SPACES.       ELPPRSSB
00337      03  SS-DETAIL-LINE12.                                        ELPPRSSB
00338          05  SS-DETAIL-LINE12-CC   PIC X(01)  VALUE ' '.          ELPPRSSB
00339          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00340          05  FILLER                PIC X(06)  VALUE 'GROUP '.     ELPPRSSB
00341          05  SS-COMMAND-GROUP      PIC X(09)  VALUE SPACES.       ELPPRSSB
00342          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00343          05  FILLER                PIC X(08)  VALUE 'SECTION '.   ELPPRSSB
00344          05  SS-COMMAND-SECTION    PIC X(05)  VALUE SPACES.       ELPPRSSB
00345          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRSSB
00346          05  FILLER                PIC X(11)  VALUE 'SUBSCRIBER '.ELPPRSSB
00347          05  SS-COMMAND-SUBSCRIBER PIC X(12)  VALUE SPACES.       ELPPRSSB
00348          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00349          05  FILLER                PIC X(10)  VALUE 'FROM DATE '. ELPPRSSB
00350          05  SS-COMMAND-FROM-DATE  PIC 99/99/99.                  ELPPRSSB
00351          05  SS-COMMAND-FROM-DATE-A REDEFINES                     ELPPRSSB
00352              SS-COMMAND-FROM-DATE  PIC X(08).                     ELPPRSSB
00353          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00354          05  FILLER                PIC X(08)  VALUE 'TO DATE '.   ELPPRSSB
00355          05  SS-COMMAND-TO-DATE    PIC 99/99/99.                  ELPPRSSB
00356          05  SS-COMMAND-TO-DATE-A REDEFINES SS-COMMAND-TO-DATE    ELPPRSSB
00357                                    PIC X(08).                     ELPPRSSB
00358          05  FILLER                PIC X(35)  VALUE SPACES.       ELPPRSSB
00359      03  SS-DETAIL-LINE13.                                        ELPPRSSB
00360          05  SS-DETAIL-LINE13-CC   PIC X(01)  VALUE '0'.          ELPPRSSB
00361          05  FILLER                PIC X(10)  VALUE 'MENU STACK'. ELPPRSSB
00362          05  FILLER                PIC X(122) VALUE SPACES.       ELPPRSSB
00363      03  SS-DETAIL-LINE14.                                        ELPPRSSB
00364          05  SS-DETAIL-LINE14-CC   PIC X(01)  VALUE ' '.          ELPPRSSB
00365          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00366          05  FILLER                PIC X(13)  VALUE               ELPPRSSB
00367              'CURRENT ITEM '.                                     ELPPRSSB
00368          05  SS-CURRENT-ITEM       PIC 9(02)  VALUE ZEROES.       ELPPRSSB
00369          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRSSB
00370          05  FILLER                PIC X(07)  VALUE 'PUSHED '.    ELPPRSSB
00371          05  SS-PUSHED             PIC X(01)  VALUE SPACES.       ELPPRSSB
00372          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00373          05  FILLER                PIC X(07)  VALUE 'STATES '.    ELPPRSSB
00374          05  SS-MENU-STATES-TABLES.                               ELPPRSSB
00375              10  SS-MENU-STATES OCCURS 10 TIMES.                  ELPPRSSB
00376                  15 SS-MENU-STATE  PIC 99.                        ELPPRSSB
00377                  15 FILLER         PIC X.                         ELPPRSSB
00378          05  FILLER                PIC X(44)  VALUE SPACES.       ELPPRSSB
00379      03  SS-DETAIL-LINE15.                                        ELPPRSSB
00380          05  SS-DETAIL-LINE15-CC   PIC X(01)  VALUE '0'.          ELPPRSSB
00381          05  FILLER                PIC X(12)  VALUE               ELPPRSSB
00382              'MENU CHOICES'.                                      ELPPRSSB
00383          05  FILLER                PIC X(03)  VALUE '  ('.        ELPPRSSB
00384          05  SS-NO-ITEMS-PRESENT   PIC 9(02)  VALUE ZERO.         ELPPRSSB
00385          05  SS-NO-ITEMS-PRESENT-A REDEFINES                      ELPPRSSB
00386                SS-NO-ITEMS-PRESENT PIC X(02).                     ELPPRSSB
00387          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00388          05  FILLER                PIC X(13)  VALUE               ELPPRSSB
00389              'ITEMS PRESENT'.                                     ELPPRSSB
00390          05  FILLER                PIC X(02)  VALUE ') '.         ELPPRSSB
00391          05  FILLER                PIC X(04)  VALUE 'FOR '.       ELPPRSSB
00392          05  SS-MENU-PHRASE        PIC X(51)  VALUE SPACES.       ELPPRSSB
00393          05  FILLER                PIC X(44)  VALUE SPACES.       ELPPRSSB
00394      03  SS-DETAIL-LINE16.                                        ELPPRSSB
00395          05  SS-DETAIL-LINE16-CC   PIC X(01)  VALUE ' '.          ELPPRSSB
00396          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00397          05  SS-CHOICES-PRESENT    PIC X(131).                    ELPPRSSB
00398      03  SS-DETAIL-LINE17.                                        ELPPRSSB
00399          05  SS-DETAIL-LINE17-CC   PIC X(01)  VALUE '0'.          ELPPRSSB
00400          05  FILLER                PIC X(29)  VALUE               ELPPRSSB
00401              'CONTRACT SUMMARY MENU CHOICES'.                     ELPPRSSB
00402          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRSSB
00403          05  FILLER                PIC X(02)  VALUE ' ('.         ELPPRSSB
00404          05  SS-CS-ITEMS-PRESENT   PIC 9(04)  VALUE ZERO.         ELPPRSSB
00405          05  FILLER                PIC X(15)  VALUE               ELPPRSSB
00406              ' ITEMS PRESENT)'.                                   ELPPRSSB
00407          05  FILLER                PIC X(80)  VALUE SPACES.       ELPPRSSB
00408      03  SS-DETAIL-LINE18.                                        ELPPRSSB
00409          05  SS-DETAIL-LINE18-CC   PIC X(01)  VALUE ' '.          ELPPRSSB
00410          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00411          05  SS-CS-CHOICES-PRESENT PIC X(130).                    ELPPRSSB
00412          05  FILLER                PIC X(01)  VALUE SPACES.       ELPPRSSB
00413      03  SS-DETAIL-LINE19.                                        ELPPRSSB
00414          05  SS-DETAIL-LINE19-CC   PIC X(01)  VALUE '0'.          ELPPRSSB
00415          05  FILLER                PIC X(20)  VALUE               ELPPRSSB
00416              'VARIATION INDICATORS'.                              ELPPRSSB
00417          05  FILLER                PIC X(112) VALUE SPACES.       ELPPRSSB
00418      03  SS-DETAIL-LINE20.                                        ELPPRSSB
00419          05  SS-DETAIL-LINE20-CC   PIC X(01)  VALUE '0'.          ELPPRSSB
00420          05  FILLER                PIC X(28)  VALUE SPACES.       ELPPRSSB
00421          05  FILLER                PIC X(05)  VALUE 'GROUP'.      ELPPRSSB
00422          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRSSB
00423          05  FILLER                PIC X(13)  VALUE               ELPPRSSB
00424              'INSTITUTIONAL'.                                     ELPPRSSB
00425          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRSSB
00426          05  FILLER                PIC X(12)  VALUE               ELPPRSSB
00427              'PROFESSIONAL'.                                      ELPPRSSB
00428          05  FILLER                PIC X(60)  VALUE SPACES.       ELPPRSSB
00429      03  SS-DETAIL-LINE21.                                        ELPPRSSB
00430          05  SS-DETAIL-LINE21-CC   PIC X(01)  VALUE ' '.          ELPPRSSB
00431          05  FILLER                PIC X(27)  VALUE SPACES.       ELPPRSSB
00432          05  FILLER                PIC X(08)  VALUE 'SPECIFIC'.   ELPPRSSB
00433          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRSSB
00434          05  FILLER                PIC X(05)  VALUE 'BASIC'.      ELPPRSSB
00435          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRSSB
00436          05  FILLER                PIC X(06)  VALUE 'SUPPL.'.     ELPPRSSB
00437          05  FILLER                PIC X(05)  VALUE SPACES.       ELPPRSSB
00438          05  FILLER                PIC X(05)  VALUE 'BASIC'.      ELPPRSSB
00439          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRSSB
00440          05  FILLER                PIC X(06)  VALUE 'SUPPL.'.     ELPPRSSB
00441          05  FILLER                PIC X(58)  VALUE SPACES.       ELPPRSSB
00442      03  SS-DETAIL-LINE22.                                        ELPPRSSB
00443          05  SS-DETAIL-LINE22-CC   PIC X(01)  VALUE ' '.          ELPPRSSB
00444          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00445          05  SS-VARIATION-IND-NAME PIC X(19).                     ELPPRSSB
00446              88 SS-MEDICARE-IND    VALUE 'MEDICARE'.              ELPPRSSB
00447              88 SS-FAMILY-REL      VALUE 'FAMILY RELATIONSHIP'.   ELPPRSSB
00448              88 SS-PAT-AGE         VALUE 'PATIENT AGE'.           ELPPRSSB
00449          05  FILLER                PIC X(09)  VALUE SPACES.       ELPPRSSB
00450          05  SS-GROUP-SPECIFIC     PIC X(01)  VALUE SPACES.       ELPPRSSB
00451          05  SS-CONTR-VAR-TABLE.                                  ELPPRSSB
00452              10  FILLER            OCCURS 4 TIMES.                ELPPRSSB
00453                  15  FILLER        PIC X(09).                     ELPPRSSB
00454                  15  SS-CONTR-VAR-IND                             ELPPRSSB
00455                                    PIC X(01).                     ELPPRSSB
00456          05  FILLER                PIC X(60)  VALUE SPACES.       ELPPRSSB
00457      03  SS-DETAIL-LINE23.                                        ELPPRSSB
00458          05  SS-DETAIL-LINE23-CC   PIC X(01)  VALUE '0'.          ELPPRSSB
00459          05  FILLER                PIC X(10)  VALUE 'RECORD IDS'. ELPPRSSB
00460          05  FILLER                PIC X(02)  VALUE SPACES.       ELPPRSSB
00461          05  FILLER                PIC X(36)  VALUE               ELPPRSSB
00462              '(VALID ONLY DURING TOPIC PROCESSING)'.              ELPPRSSB
00463          05  FILLER                PIC X(84)  VALUE SPACES.       ELPPRSSB
00464      03  SS-DETAIL-LINE24.                                        ELPPRSSB
00465          05  SS-DETAIL-LINE24-CC   PIC X(01)  VALUE '0'.          ELPPRSSB
00466          05  FILLER                PIC X(37)  VALUE SPACES.       ELPPRSSB
00467          05  FILLER                PIC X(08)  VALUE 'PROVIDER'.   ELPPRSSB
00468          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRSSB
00469          05  FILLER                PIC X(07)  VALUE 'FAM REL'.    ELPPRSSB
00470          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRSSB
00471          05  FILLER                PIC X(03)  VALUE 'EFF'.        ELPPRSSB
00472          05  FILLER                PIC X(05)  VALUE SPACES.       ELPPRSSB
00473          05  FILLER                PIC X(04)  VALUE 'TERM'.       ELPPRSSB
00474          05  FILLER                PIC X(58)  VALUE SPACES.       ELPPRSSB
00475      03  SS-DETAIL-LINE25.                                        ELPPRSSB
00476          05  SS-DETAIL-LINE25-CC   PIC X(01)  VALUE ' '.          ELPPRSSB
00477          05  FILLER                PIC X(27)  VALUE SPACES.       ELPPRSSB
00478          05  FILLER                PIC X(06)  VALUE 'L.O.B.'.     ELPPRSSB
00479          05  FILLER                PIC X(05)  VALUE SPACES.       ELPPRSSB
00480          05  FILLER                PIC X(07)  VALUE 'CONTROL'.    ELPPRSSB
00481          05  FILLER                PIC X(07)  VALUE SPACES.       ELPPRSSB
00482          05  FILLER                PIC X(05)  VALUE 'LEVEL'.      ELPPRSSB
00483          05  FILLER                PIC X(05)  VALUE SPACES.       ELPPRSSB
00484          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRSSB
00485          05  FILLER                PIC X(04)  VALUE SPACES.       ELPPRSSB
00486          05  FILLER                PIC X(04)  VALUE 'DATE'.       ELPPRSSB
00487          05  FILLER                PIC X(58)  VALUE SPACES.       ELPPRSSB
00488      03  SS-DETAIL-LINE26.                                        ELPPRSSB
00489          05  SS-DETAIL-LINE26-CC   PIC X(01)  VALUE ' '.          ELPPRSSB
00490          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00491          05  SS-RECORD-ID-NAME     PIC X(14).                     ELPPRSSB
00492              88 SS-GRP-SPECIFIC-ID VALUE 'GROUP SPECIFIC'.        ELPPRSSB
00493              88 SS-INBA-IND        VALUE 'INST BASIC'.            ELPPRSSB
00494              88 SS-SUPRV-IND       VALUE 'INST SUPPL'.            ELPPRSSB
00495              88 SS-PRBA-IND        VALUE 'PROF BASIC'.            ELPPRSSB
00496              88 SS-PRSU-IND        VALUE 'PROF SUPPL'.            ELPPRSSB
00497          05  FILLER                PIC X(12)  VALUE SPACES.       ELPPRSSB
00498          05  SS-LOB                PIC X(01)  VALUE '-'.          ELPPRSSB
00499          05  FILLER                PIC X(11)  VALUE SPACES.       ELPPRSSB
00500          05  SS-PROV-CONTROL       PIC X(02)  VALUE '--'.         ELPPRSSB
00501          05  FILLER                PIC X(11)  VALUE SPACES.       ELPPRSSB
00502          05  SS-FAM-RELATIONSHIP   PIC X(02)  VALUE SPACES.       ELPPRSSB
00503          05  FILLER                PIC X(06)  VALUE SPACES.       ELPPRSSB
00504          05  SS-EFF-DATE           PIC 9(07)  VALUE ZEROES.       ELPPRSSB
00505          05  SS-EFF-DATE-A REDEFINES                              ELPPRSSB
00506                SS-EFF-DATE         PIC X(07).                     ELPPRSSB
00507          05  FILLER                PIC X(03)  VALUE SPACES.       ELPPRSSB
00508          05  SS-TERM-DATE          PIC 9(07)  VALUE ZEROES.       ELPPRSSB
00509          05  SS-TERM-DATE-A REDEFINES                             ELPPRSSB
00510                SS-TERM-DATE        PIC X(07).                     ELPPRSSB
00511          05  FILLER                PIC X(57)  VALUE SPACES.       ELPPRSSB
00512 /                                                                 ELPPRSSB
00513      COPY ELSTCWAC.                                               ELPPRSSB
00514                                                                   ELPPRSSB
00515  01  FILLER           PIC X(16)   VALUE '*END OF ELPPRSSB'.       ELPPRSSB
00516 /                                                                 ELPPRSSB
00517  LINKAGE SECTION.                                                 ELPPRSSB
00518  COPY ELSPRCBC.                                                   ELPPRSSB
00519 /                                                                 ELPPRSSB
00520  COPY ELSSSCBC.                                                   ELPPRSSB
00521 /                                                                 ELPPRSSB
00522  PROCEDURE DIVISION USING PCB-PRINT-CONTROL-BLOCK                 ELPPRSSB
00523                           SSB-SELECTOR-STATUS-CTL-BLK.            ELPPRSSB
00524      SET WS-NO-INVALID-DATA-FOUND TO TRUE.                        ELPPRSSB
00525      PERFORM 0000-PRINT-SSB-LINES.                                ELPPRSSB
00526      IF WS-INVALID-DATA-FOUND                                     ELPPRSSB
00527          SET PCB-INVALID-DATA-FOUND TO TRUE                       ELPPRSSB
00528      ELSE                                                         ELPPRSSB
00529          SET PCB-OK TO TRUE.                                      ELPPRSSB
00530      GOBACK.                                                      ELPPRSSB
00531      SKIP3                                                        ELPPRSSB
00532  0000-PRINT-SSB-LINES.                                            ELPPRSSB
00533      PERFORM 1000-PRINT-ABEND-DESC.                               ELPPRSSB
00534      PERFORM 1010-PRINT-SELECTION-STATUS.                         ELPPRSSB
00535      PERFORM 1100-PRINT-SELECTION-CRITERIA.                       ELPPRSSB
00536      IF SSB-CMDLN-DATA NOT EQUAL LOW-VALUES                       ELPPRSSB
00537          PERFORM 1200-PRINT-COMMAND-LINE.                         ELPPRSSB
00538      IF NOT SSB-STACK-EMPTY                                       ELPPRSSB
00539          PERFORM 1300-PRINT-MENU-STACK.                           ELPPRSSB
00540      IF SSB-MNU-NUM-CHOICES > ZERO                                ELPPRSSB
00541          PERFORM 1400-PRINT-MENU-CHOICES.                         ELPPRSSB
00542      IF SSB-CS-MNU-RESPONSE-TABLE NOT EQUAL LOW-VALUES            ELPPRSSB
00543          PERFORM 1500-PRINT-CS-MENU-CHOICES.                      ELPPRSSB
00544      PERFORM 1600-PRINT-VAR-IND.                                  ELPPRSSB
00545      IF SSB-SS-SELECTION-DONE                                     ELPPRSSB
00546          PERFORM 1700-PRINT-RECORD-IDS.                           ELPPRSSB
00547      SKIP3                                                        ELPPRSSB
00548  1000-PRINT-ABEND-DESC.                                           ELPPRSSB
00549      SEARCH ALL WS-ABEND-MSG-REDEFINES                            ELPPRSSB
00550          AT END                                                   ELPPRSSB
00551               MOVE 'UNDEFINED OR SYSTEM ABEND' TO                 ELPPRSSB
00552                    SS-ABEND-DESC                                  ELPPRSSB
00553          WHEN WS-ABEND-CODE (WS-ABEND-IDX) = PCB-ABEND-CODE       ELPPRSSB
00554               MOVE WS-ABEND-DESC (WS-ABEND-IDX) TO                ELPPRSSB
00555                    SS-ABEND-DESC                                  ELPPRSSB
00556      END-SEARCH.                                                  ELPPRSSB
00557      MOVE SS-DETAIL-LINE1A          TO PCB-PRINT-AREA.            ELPPRSSB
00558      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00559      SKIP3                                                        ELPPRSSB
00560  1010-PRINT-SELECTION-STATUS.                                     ELPPRSSB
00561      MOVE SS-DETAIL-LINE1B          TO PCB-PRINT-AREA.            ELPPRSSB
00562      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00563      MOVE SSB-SELECTOR-STATE        TO SS-SELECTOR-STATE.         ELPPRSSB
00564      MOVE SSB-MODULE-STATUS-TABLE   TO SS-STATUS-AREA.            ELPPRSSB
00565      MOVE SSB-ACTION-MODULE         TO SS-MODULE.                 ELPPRSSB
00566      MOVE SS-DETAIL-LINE2           TO PCB-PRINT-AREA.            ELPPRSSB
00567      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00568 /                                                                 ELPPRSSB
00569  1100-PRINT-SELECTION-CRITERIA.                                   ELPPRSSB
00570      PERFORM 1110-PRINT-HEADING-LINE.                             ELPPRSSB
00571      PERFORM 1120-PRINT-SUBSCRIBER-ID.                            ELPPRSSB
00572      PERFORM 1130-PRINT-TOPIC-LINES.                              ELPPRSSB
00573                                                                   ELPPRSSB
00574  1110-PRINT-HEADING-LINE.                                         ELPPRSSB
00575      IF SSB-COVRD-FROM-DT IS NUMERIC                              ELPPRSSB
00576          MOVE SSB-COVRD-FROM-DT TO WS-JULIAN-DATE                 ELPPRSSB
00577          PERFORM 9010-CONVERT-DATE                                ELPPRSSB
00578          MOVE WS-GREGORIAN-DATE TO SS-COV-FROM-DATE               ELPPRSSB
00579      ELSE                                                         ELPPRSSB
00580          MOVE ALL '?' TO SS-COV-FROM-DATE-A                       ELPPRSSB
00581          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00582      END-IF.                                                      ELPPRSSB
00583                                                                   ELPPRSSB
00584      IF SSB-COVRD-TO-DT IS NUMERIC                                ELPPRSSB
00585          MOVE SSB-COVRD-TO-DT TO WS-JULIAN-DATE                   ELPPRSSB
00586          PERFORM 9010-CONVERT-DATE                                ELPPRSSB
00587          MOVE WS-GREGORIAN-DATE TO SS-COV-TO-DATE                 ELPPRSSB
00588      ELSE                                                         ELPPRSSB
00589          MOVE ALL '?' TO SS-COV-TO-DATE-A                         ELPPRSSB
00590          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00591      END-IF.                                                      ELPPRSSB
00592                                                                   ELPPRSSB
00593      MOVE SS-DETAIL-LINE3           TO PCB-PRINT-AREA.            ELPPRSSB
00594      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00595                                                                   ELPPRSSB
00596  1120-PRINT-SUBSCRIBER-ID.                                        ELPPRSSB
00597      PERFORM 1122-PRINT-ID-LINE-1.                                ELPPRSSB
00598      PERFORM 1124-PRINT-ID-LINE-2.                                ELPPRSSB
00599      PERFORM 1126-PRINT-ID-LINE-3.                                ELPPRSSB
00600                                                                   ELPPRSSB
00601  1122-PRINT-ID-LINE-1.                                            ELPPRSSB
00602      IF SSB-NO-PLAN-CODE                                          ELPPRSSB
00603          MOVE SPACES          TO SS-PLAN-CODE-LINE4               ELPPRSSB
00604      ELSE                                                         ELPPRSSB
00605          MOVE 000             TO SS-PLAN-CODE-LINE4               ELPPRSSB
00606      END-IF.                                                      ELPPRSSB
00607      IF SSB-NO-GRP-NO                                             ELPPRSSB
00608          MOVE SPACES          TO SS-GROUP-NO-LINE4                ELPPRSSB
00609      ELSE                                                         ELPPRSSB
00610          MOVE SSB-GROUP-NUMBER TO SS-GROUP-NO-LINE4               ELPPRSSB
00611      END-IF.                                                      ELPPRSSB
00612      IF SSB-NO-SECTN-NO                                           ELPPRSSB
00613          MOVE SPACES          TO SS-SECTION-NO-LINE4              ELPPRSSB
00614      ELSE                                                         ELPPRSSB
00615          MOVE SSB-SECTN-NO    TO SS-SECTION-NO-LINE4              ELPPRSSB
00616      END-IF.                                                      ELPPRSSB
00617      IF SSB-NO-PKG-CODE                                           ELPPRSSB
00618          MOVE SPACES          TO SS-PKG-CODE-NO-LINE4             ELPPRSSB
00619      ELSE                                                         ELPPRSSB
00620          MOVE SSB-PKG-CODE    TO SS-PKG-CODE-NO-LINE4             ELPPRSSB
00621      END-IF.                                                      ELPPRSSB
00622                                                                   ELPPRSSB
00623      IF SSB-SRV-FROM-DT-CEN IS NUMERIC                            ELPPRSSB
00624          MOVE SSB-SRV-FROM-DATE TO WS-JULIAN-DATE                 ELPPRSSB
00625          PERFORM 9010-CONVERT-DATE                                ELPPRSSB
00626          MOVE WS-GREGORIAN-DATE TO SS-SERVICE-FROM-DATE           ELPPRSSB
00627      ELSE                                                         ELPPRSSB
00628          MOVE ALL '?' TO SS-SERVICE-FROM-DATE-A                   ELPPRSSB
00629          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00630      END-IF.                                                      ELPPRSSB
00631                                                                   ELPPRSSB
00632      IF SSB-SRV-TO-DT-CEN IS NUMERIC                              ELPPRSSB
00633          MOVE SSB-SRV-TO-DATE TO WS-JULIAN-DATE                   ELPPRSSB
00634          PERFORM 9010-CONVERT-DATE                                ELPPRSSB
00635          MOVE WS-GREGORIAN-DATE TO SS-SERVICE-TO-DATE             ELPPRSSB
00636      ELSE                                                         ELPPRSSB
00637          MOVE ALL '?' TO SS-SERVICE-TO-DATE-A                     ELPPRSSB
00638          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00639      END-IF.                                                      ELPPRSSB
00640                                                                   ELPPRSSB
00641      EVALUATE TRUE                                                ELPPRSSB
00642      WHEN SSB-PROV-CLASS-INST                                     ELPPRSSB
00643           MOVE 'INST'  TO SS-PROVIDER-CLASS                       ELPPRSSB
00644      WHEN SSB-PROV-CLASS-PROF                                     ELPPRSSB
00645           MOVE 'PROF'  TO SS-PROVIDER-CLASS                       ELPPRSSB
00646      WHEN SSB-PROV-CLASS-BOTH                                     ELPPRSSB
00647           MOVE 'ALL '  TO SS-PROVIDER-CLASS                       ELPPRSSB
00648      WHEN SSB-PROVIDER-CLASS = LOW-VALUES                         ELPPRSSB
00649           MOVE '____'  TO SS-PROVIDER-CLASS                       ELPPRSSB
00650      WHEN OTHER                                                   ELPPRSSB
00651           MOVE ALL '?' TO SS-PROVIDER-CLASS                       ELPPRSSB
00652           SET WS-INVALID-DATA-FOUND TO TRUE                       ELPPRSSB
00653      END-EVALUATE.                                                ELPPRSSB
00654                                                                   ELPPRSSB
00655      EVALUATE TRUE                                                ELPPRSSB
00656      WHEN SSB-SERV-CLASS-IP                                       ELPPRSSB
00657           MOVE 'INPT'  TO SS-SERVICE-CLASS                        ELPPRSSB
00658      WHEN SSB-SERV-CLASS-OP                                       ELPPRSSB
00659           MOVE 'OUT '  TO SS-SERVICE-CLASS                        ELPPRSSB
00660      WHEN SSB-SERV-CLASS-BOTH                                     ELPPRSSB
00661           MOVE 'ALL '  TO SS-SERVICE-CLASS                        ELPPRSSB
00662      WHEN SSB-SERVICE-CLASS = LOW-VALUES                          ELPPRSSB
00663           MOVE '____'  TO SS-SERVICE-CLASS                        ELPPRSSB
00664      WHEN OTHER                                                   ELPPRSSB
00665           MOVE ALL '?' TO SS-SERVICE-CLASS                        ELPPRSSB
00666           SET WS-INVALID-DATA-FOUND TO TRUE                       ELPPRSSB
00667      END-EVALUATE.                                                ELPPRSSB
00668                                                                   ELPPRSSB
00669      MOVE SS-DETAIL-LINE4           TO PCB-PRINT-AREA.            ELPPRSSB
00670      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00671                                                                   ELPPRSSB
00672  1124-PRINT-ID-LINE-2.                                            ELPPRSSB
00673      EVALUATE TRUE                                                ELPPRSSB
00674      WHEN SSB-MEDCA-ELIG                                          ELPPRSSB
00675           MOVE 'YES' TO SS-MEDICARE                               ELPPRSSB
00676      WHEN SSB-MEDCA-INELIG                                        ELPPRSSB
00677           MOVE 'NO ' TO SS-MEDICARE                               ELPPRSSB
00678      WHEN SSB-MEDCA-UNDEF                                         ELPPRSSB
00679           MOVE 'N/A' TO SS-MEDICARE                               ELPPRSSB
00680      WHEN OTHER                                                   ELPPRSSB
00681           MOVE '???' TO SS-MEDICARE                               ELPPRSSB
00682           SET WS-INVALID-DATA-FOUND TO TRUE                       ELPPRSSB
00683      END-EVALUATE.                                                ELPPRSSB
00684                                                                   ELPPRSSB
00685      MOVE SSB-PT-AGE TO SS-PATIENT-AGE.                           ELPPRSSB
00686      MOVE SSB-FAM-REL TO SS-FAM-REL.                              ELPPRSSB
00687                                                                   ELPPRSSB
00688      MOVE SS-DETAIL-LINE5  TO  PCB-PRINT-AREA.                    ELPPRSSB
00689      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00690                                                                   ELPPRSSB
00691  1126-PRINT-ID-LINE-3.                                            ELPPRSSB
00692      IF SSB-NO-SUBSCRIBER-NBR                                     ELPPRSSB
00693          MOVE 'N/A'   TO SS-SUBSCRIBER                            ELPPRSSB
00694          MOVE ALL '_' TO SS-EFFECTIVE-DATE-A                      ELPPRSSB
00695                          SS-TERMINATE-DATE-A                      ELPPRSSB
00696      ELSE                                                         ELPPRSSB
00697          MOVE SSB-SUBSCRIBER-NBR TO SS-SUBSCRIBER                 ELPPRSSB
00698          PERFORM 1127-PRINT-SUB-DATES                             ELPPRSSB
00699      END-IF.                                                      ELPPRSSB
00700      MOVE SS-DETAIL-LINE6  TO  PCB-PRINT-AREA.                    ELPPRSSB
00701      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00702                                                                   ELPPRSSB
00703  1127-PRINT-SUB-DATES.                                            ELPPRSSB
00704      IF SSB-SUB-SECTN-EFF-DT IS NUMERIC                           ELPPRSSB
00705          MOVE SSB-SUB-SECTN-EFF-DT TO WS-WORK-DATE                ELPPRSSB
00706          MOVE WS-WORK-JUL TO WS-JULIAN-DATE                       ELPPRSSB
00707          PERFORM 9010-CONVERT-DATE                                ELPPRSSB
00708          MOVE WS-GREGORIAN-DATE TO SS-EFFECTIVE-DATE              ELPPRSSB
00709      ELSE                                                         ELPPRSSB
00710          MOVE ALL '?' TO SS-EFFECTIVE-DATE-A                      ELPPRSSB
00711          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00712      END-IF.                                                      ELPPRSSB
00713                                                                   ELPPRSSB
00714      IF SSB-SUB-SECTN-TERMN-DT IS NUMERIC                         ELPPRSSB
00715          MOVE SSB-SUB-SECTN-TERMN-DT TO WS-WORK-DATE              ELPPRSSB
00716          MOVE WS-WORK-JUL TO WS-JULIAN-DATE                       ELPPRSSB
00717          PERFORM 9010-CONVERT-DATE                                ELPPRSSB
00718          MOVE WS-GREGORIAN-DATE TO SS-TERMINATE-DATE              ELPPRSSB
00719      ELSE                                                         ELPPRSSB
00720          MOVE ALL '?' TO SS-TERMINATE-DATE-A                      ELPPRSSB
00721          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00722      END-IF.                                                      ELPPRSSB
00723                                                                   ELPPRSSB
00724  1130-PRINT-TOPIC-LINES.                                          ELPPRSSB
00725      MOVE SSB-TOPIC            TO SS-TOPIC.                       ELPPRSSB
00726      MOVE SSB-TOPIC-PHRASE     TO SS-TOPIC-PHRASE.                ELPPRSSB
00727      MOVE SS-DETAIL-LINE7      TO PCB-PRINT-AREA.                 ELPPRSSB
00728      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00729      MOVE SSB-SUB-TOPIC        TO SS-SUB-TOPIC.                   ELPPRSSB
00730      MOVE SSB-SUB-TOPIC-PHRASE TO SS-SUB-TOPIC-PHRASE.            ELPPRSSB
00731      MOVE SS-DETAIL-LINE8      TO PCB-PRINT-AREA.                 ELPPRSSB
00732      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00733      MOVE SSB-MODIFIER-1        TO SS-MODIFIER-1.                 ELPPRSSB
00734      MOVE SSB-MODIFIER-1-PHRASE TO SS-MODIFIER-PHRASE1.           ELPPRSSB
00735      MOVE SS-DETAIL-LINE9       TO PCB-PRINT-AREA.                ELPPRSSB
00736      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00737      MOVE SSB-MODIFIER-2        TO SS-MODIFIER-2.                 ELPPRSSB
00738      MOVE SSB-MODIFIER-2-PHRASE TO SS-MODIFIER-PHRASE2            ELPPRSSB
00739      MOVE SS-DETAIL-LINE10      TO PCB-PRINT-AREA.                ELPPRSSB
00740      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00741 /                                                                 ELPPRSSB
00742  1200-PRINT-COMMAND-LINE.                                         ELPPRSSB
00743      MOVE SS-DETAIL-LINE11   TO PCB-PRINT-AREA.                   ELPPRSSB
00744      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00745      MOVE SSB-CMDLN-GRP-NO   TO SS-COMMAND-GROUP.                 ELPPRSSB
00746      MOVE SSB-CMDLN-SECTN-NO TO SS-COMMAND-SECTION.               ELPPRSSB
00747      MOVE SSB-CMDLN-SUBSCRIBER-NBR                                ELPPRSSB
00748                              TO SS-COMMAND-SUBSCRIBER.            ELPPRSSB
00749                                                                   ELPPRSSB
00750      IF SSB-CMDLN-SRV-FROM-DT-CC IS NUMERIC                       ELPPRSSB
00751          MOVE SSB-CMDLN-SRV-FROM-DT TO WS-JULIAN-DATE             ELPPRSSB
00752          PERFORM 9010-CONVERT-DATE                                ELPPRSSB
00753          MOVE WS-GREGORIAN-DATE TO SS-COMMAND-FROM-DATE           ELPPRSSB
00754      ELSE                                                         ELPPRSSB
00755          MOVE ALL '?' TO SS-COMMAND-FROM-DATE-A                   ELPPRSSB
00756          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00757      END-IF.                                                      ELPPRSSB
00758                                                                   ELPPRSSB
00759      IF SSB-CMDLN-SRV-TO-DATE-CC IS NUMERIC                       ELPPRSSB
00760          MOVE SSB-CMDLN-SRV-TO-DT TO WS-JULIAN-DATE               ELPPRSSB
00761          PERFORM 9010-CONVERT-DATE                                ELPPRSSB
00762          MOVE WS-GREGORIAN-DATE TO SS-COMMAND-TO-DATE             ELPPRSSB
00763      ELSE                                                         ELPPRSSB
00764          MOVE ALL '?' TO SS-COMMAND-TO-DATE-A                     ELPPRSSB
00765          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00766      END-IF.                                                      ELPPRSSB
00767                                                                   ELPPRSSB
00768      INSPECT SS-DETAIL-LINE12 REPLACING ALL LOW-VALUES            ELPPRSSB
00769                               BY SPACES.                          ELPPRSSB
00770      MOVE SS-DETAIL-LINE12   TO PCB-PRINT-AREA.                   ELPPRSSB
00771      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00772 /                                                                 ELPPRSSB
00773  1300-PRINT-MENU-STACK.                                           ELPPRSSB
00774      MOVE SS-DETAIL-LINE13   TO PCB-PRINT-AREA.                   ELPPRSSB
00775      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00776                                                                   ELPPRSSB
00777      MOVE SSB-STACK-CURRENT-ITEM TO SS-CURRENT-ITEM.              ELPPRSSB
00778      MOVE SSB-PUSH-INDICATOR TO SS-PUSHED.                        ELPPRSSB
00779      MOVE SPACES TO SS-MENU-STATES-TABLES.                        ELPPRSSB
00780      IF SSB-VALID-STACK-ITEM                                      ELPPRSSB
00781          MOVE 1 TO WS-SUB                                         ELPPRSSB
00782          PERFORM UNTIL WS-SUB > SSB-STACK-CURRENT-ITEM            ELPPRSSB
00783              MOVE SSB-STACK-STATE (WS-SUB) TO                     ELPPRSSB
00784                   SS-MENU-STATE (WS-SUB)                          ELPPRSSB
00785              ADD 1 TO WS-SUB                                      ELPPRSSB
00786          END-PERFORM                                              ELPPRSSB
00787      ELSE                                                         ELPPRSSB
00788          MOVE '*** INVALID CURRENT STACK COUNT ***' TO            ELPPRSSB
00789               SS-MENU-STATES-TABLES                               ELPPRSSB
00790      END-IF.                                                      ELPPRSSB
00791                                                                   ELPPRSSB
00792      MOVE SS-DETAIL-LINE14   TO PCB-PRINT-AREA.                   ELPPRSSB
00793      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00794      SKIP3                                                        ELPPRSSB
00795  1400-PRINT-MENU-CHOICES.                                         ELPPRSSB
00796      MOVE SSB-MNU-NUM-CHOICES  TO SS-NO-ITEMS-PRESENT.            ELPPRSSB
00797      MOVE SSB-MNU-TITLE        TO SS-MENU-PHRASE.                 ELPPRSSB
00798      MOVE SS-DETAIL-LINE15     TO PCB-PRINT-AREA.                 ELPPRSSB
00799      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00800                                                                   ELPPRSSB
00801      MOVE 1 TO WS-SUB.                                            ELPPRSSB
00802      MOVE 1 TO TCAR-AREA-LENGTH.                                  ELPPRSSB
00803      MOVE SPACES TO TCAR-FROM-AREA.                               ELPPRSSB
00804      PERFORM UNTIL WS-SUB > SSB-MNU-NUM-CHOICES                   ELPPRSSB
00805           STRING SSB-MNU-CHOICE (WS-SUB) DELIMITED BY SIZE        ELPPRSSB
00806                  ' '                     DELIMITED BY SIZE        ELPPRSSB
00807               INTO TCAR-FROM-AREA                                 ELPPRSSB
00808               POINTER TCAR-AREA-LENGTH                            ELPPRSSB
00809           END-STRING                                              ELPPRSSB
00810           ADD 1 TO WS-SUB                                         ELPPRSSB
00811      END-PERFORM.                                                 ELPPRSSB
00812      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELPPRSSB
00813      MOVE TCAR-TO-AREA TO SS-CHOICES-PRESENT.                     ELPPRSSB
00814                                                                   ELPPRSSB
00815      MOVE SS-DETAIL-LINE16     TO PCB-PRINT-AREA.                 ELPPRSSB
00816      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00817 /                                                                 ELPPRSSB
00818  1500-PRINT-CS-MENU-CHOICES.                                      ELPPRSSB
00819      MOVE 1 TO WS-SUB.                                            ELPPRSSB
00820      MOVE 1 TO TCAR-AREA-LENGTH.                                  ELPPRSSB
00821      MOVE SPACES TO TCAR-FROM-AREA.                               ELPPRSSB
00822      PERFORM UNTIL WS-SUB > 10                                    ELPPRSSB
00823                 OR (SSB-CS-RESPONSE (WS-SUB) = SPACES             ELPPRSSB
00824                     OR LOW-VALUES)                                ELPPRSSB
00825           STRING SSB-CS-RESPONSE (WS-SUB) DELIMITED BY SIZE       ELPPRSSB
00826                  ' '                      DELIMITED BY SIZE       ELPPRSSB
00827               INTO TCAR-FROM-AREA                                 ELPPRSSB
00828               POINTER TCAR-AREA-LENGTH                            ELPPRSSB
00829           END-STRING                                              ELPPRSSB
00830           ADD 1 TO WS-SUB                                         ELPPRSSB
00831      END-PERFORM.                                                 ELPPRSSB
00832      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELPPRSSB
00833      MOVE TCAR-TO-AREA TO SS-CS-CHOICES-PRESENT.                  ELPPRSSB
00834                                                                   ELPPRSSB
00835      COMPUTE SS-CS-ITEMS-PRESENT = WS-SUB - 1.                    ELPPRSSB
00836      MOVE SS-DETAIL-LINE17     TO PCB-PRINT-AREA.                 ELPPRSSB
00837      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00838                                                                   ELPPRSSB
00839      MOVE SS-DETAIL-LINE18     TO PCB-PRINT-AREA.                 ELPPRSSB
00840      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00841 /                                                                 ELPPRSSB
00842  1600-PRINT-VAR-IND.                                              ELPPRSSB
00843      PERFORM 1610-PRINT-VAR-HEADINGS.                             ELPPRSSB
00844      PERFORM 1620-PRINT-MEDC-VAR.                                 ELPPRSSB
00845      PERFORM 1630-PRINT-FAMR-VAR.                                 ELPPRSSB
00846      PERFORM 1640-PRINT-PTAG-VAR.                                 ELPPRSSB
00847                                                                   ELPPRSSB
00848  1610-PRINT-VAR-HEADINGS.                                         ELPPRSSB
00849      MOVE SS-DETAIL-LINE19     TO PCB-PRINT-AREA.                 ELPPRSSB
00850      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00851      MOVE SS-DETAIL-LINE20     TO PCB-PRINT-AREA.                 ELPPRSSB
00852      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00853      MOVE SS-DETAIL-LINE21     TO PCB-PRINT-AREA.                 ELPPRSSB
00854      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00855                                                                   ELPPRSSB
00856  1620-PRINT-MEDC-VAR.                                             ELPPRSSB
00857      SET SS-MEDICARE-IND       TO TRUE.                           ELPPRSSB
00858      MOVE SSB-FR-MED-GRP       TO SS-GROUP-SPECIFIC.              ELPPRSSB
00859      MOVE SPACES TO SS-CONTR-VAR-TABLE.                           ELPPRSSB
00860      PERFORM VARYING WS-SUB FROM 1 BY 1                           ELPPRSSB
00861            UNTIL WS-SUB > 4                                       ELPPRSSB
00862          MOVE SSB-FR-MED-CONT  (WS-SUB)                           ELPPRSSB
00863            TO SS-CONTR-VAR-IND (WS-SUB)                           ELPPRSSB
00864      END-PERFORM.                                                 ELPPRSSB
00865      MOVE SS-DETAIL-LINE22     TO PCB-PRINT-AREA.                 ELPPRSSB
00866      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00867                                                                   ELPPRSSB
00868  1630-PRINT-FAMR-VAR.                                             ELPPRSSB
00869      SET SS-FAMILY-REL         TO TRUE.                           ELPPRSSB
00870      MOVE SSB-FR-FR-GRP        TO SS-GROUP-SPECIFIC.              ELPPRSSB
00871      MOVE SPACES TO SS-CONTR-VAR-TABLE.                           ELPPRSSB
00872      PERFORM VARYING WS-SUB FROM 1 BY 1                           ELPPRSSB
00873            UNTIL WS-SUB > 4                                       ELPPRSSB
00874          MOVE SSB-FR-FR-CONT   (WS-SUB)                           ELPPRSSB
00875            TO SS-CONTR-VAR-IND (WS-SUB)                           ELPPRSSB
00876      END-PERFORM.                                                 ELPPRSSB
00877      MOVE SS-DETAIL-LINE22     TO PCB-PRINT-AREA.                 ELPPRSSB
00878      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00879                                                                   ELPPRSSB
00880  1640-PRINT-PTAG-VAR.                                             ELPPRSSB
00881      SET SS-PAT-AGE            TO TRUE.                           ELPPRSSB
00882      MOVE SSB-FR-PT-AGE-GRP    TO SS-GROUP-SPECIFIC.              ELPPRSSB
00883      MOVE SPACES TO SS-CONTR-VAR-TABLE.                           ELPPRSSB
00884      PERFORM VARYING WS-SUB FROM 1 BY 1                           ELPPRSSB
00885            UNTIL WS-SUB > 4                                       ELPPRSSB
00886          MOVE SSB-FR-PT-AGE-CONT   (WS-SUB)                       ELPPRSSB
00887            TO SS-CONTR-VAR-IND     (WS-SUB)                       ELPPRSSB
00888      END-PERFORM.                                                 ELPPRSSB
00889      MOVE SS-DETAIL-LINE22     TO PCB-PRINT-AREA.                 ELPPRSSB
00890      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00891 /                                                                 ELPPRSSB
00892  1700-PRINT-RECORD-IDS.                                           ELPPRSSB
00893      PERFORM 1710-PRINT-RECORD-HEADING.                           ELPPRSSB
00894      PERFORM 1720-PRINT-GROUP-KEY.                                ELPPRSSB
00895      PERFORM 1730-PRINT-CONTRACT-KEY                              ELPPRSSB
00896         VARYING WS-SUB FROM 1 BY 1                                ELPPRSSB
00897           UNTIL WS-SUB > 4.                                       ELPPRSSB
00898                                                                   ELPPRSSB
00899  1710-PRINT-RECORD-HEADING.                                       ELPPRSSB
00900      MOVE SS-DETAIL-LINE23     TO PCB-PRINT-AREA.                 ELPPRSSB
00901      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00902      MOVE SS-DETAIL-LINE24     TO PCB-PRINT-AREA.                 ELPPRSSB
00903      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00904      MOVE SS-DETAIL-LINE25     TO PCB-PRINT-AREA.                 ELPPRSSB
00905      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00906                                                                   ELPPRSSB
00907  1720-PRINT-GROUP-KEY.                                            ELPPRSSB
00908      SET SS-GRP-SPECIFIC-ID    TO TRUE.                           ELPPRSSB
00909      MOVE '-'                  TO SS-LOB.                         ELPPRSSB
00910      MOVE '--'                 TO SS-PROV-CONTROL.                ELPPRSSB
00911      MOVE SSB-GRP-FAM-REL-LVL  TO SS-FAM-RELATIONSHIP.            ELPPRSSB
00912      IF SSB-GROUP-EFF-DATE-CEN IS NUMERIC                         ELPPRSSB
00913          MOVE SSB-GROUP-EFF-DATE-CEN TO SS-EFF-DATE               ELPPRSSB
00914      ELSE                                                         ELPPRSSB
00915          MOVE ALL '?'          TO SS-EFF-DATE-A                   ELPPRSSB
00916          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00917      END-IF.                                                      ELPPRSSB
00918      IF SSB-GROUP-TERM-DATE-CEN IS NUMERIC                        ELPPRSSB
00919          MOVE SSB-GROUP-TERM-DATE-CEN TO SS-TERM-DATE             ELPPRSSB
00920      ELSE                                                         ELPPRSSB
00921          MOVE ALL '?'          TO SS-TERM-DATE-A                  ELPPRSSB
00922          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00923      END-IF.                                                      ELPPRSSB
00924      MOVE SS-DETAIL-LINE26     TO PCB-PRINT-AREA.                 ELPPRSSB
00925      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00926 /                                                                 ELPPRSSB
00927  1730-PRINT-CONTRACT-KEY.                                         ELPPRSSB
00928      MOVE WS-CONTRACT-TYPE (WS-SUB)                               ELPPRSSB
00929                                   TO SS-RECORD-ID-NAME.           ELPPRSSB
00930      IF SSB-CONT-L-O-B (WS-SUB) = LOW-VALUE  OR                   ELPPRSSB
00931         SSB-CONT-FAM-REL-LVL (WS-SUB) = LOW-VALUE                 ELPPRSSB
00932          PERFORM 1740-CLEAR-PRINT-ITEMS                           ELPPRSSB
00933      ELSE                                                         ELPPRSSB
00934          PERFORM 1750-MOVE-PRINT-ITEMS.                           ELPPRSSB
00935      MOVE SS-DETAIL-LINE26        TO PCB-PRINT-AREA.              ELPPRSSB
00936      PERFORM 9000-PRINT-LINE.                                     ELPPRSSB
00937                                                                   ELPPRSSB
00938  1740-CLEAR-PRINT-ITEMS.                                          ELPPRSSB
00939      MOVE SPACES TO SS-LOB                                        ELPPRSSB
00940                     SS-PROV-CONTROL                               ELPPRSSB
00941                     SS-FAM-RELATIONSHIP                           ELPPRSSB
00942                     SS-EFF-DATE-A                                 ELPPRSSB
00943                     SS-TERM-DATE-A.                               ELPPRSSB
00944                                                                   ELPPRSSB
00945  1750-MOVE-PRINT-ITEMS.                                           ELPPRSSB
00946      MOVE SSB-CONT-L-O-B (WS-SUB) TO SS-LOB.                      ELPPRSSB
00947      MOVE SSB-CONT-PROVDR-CONTROL (WS-SUB)                        ELPPRSSB
00948                                   TO SS-PROV-CONTROL.             ELPPRSSB
00949      MOVE SSB-CONT-FAM-REL-LVL (WS-SUB)                           ELPPRSSB
00950                                   TO SS-FAM-RELATIONSHIP.         ELPPRSSB
00951      IF SSB-CONT-EFF-DATE-CEN (WS-SUB) IS NUMERIC                 ELPPRSSB
00952          MOVE SSB-CONT-EFF-DATE-CEN (WS-SUB) TO SS-EFF-DATE       ELPPRSSB
00953      ELSE                                                         ELPPRSSB
00954          MOVE ALL '?'                TO SS-EFF-DATE-A             ELPPRSSB
00955          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00956      END-IF.                                                      ELPPRSSB
00957      IF SSB-CONT-TERMN-DATE-CEN (WS-SUB) IS NUMERIC               ELPPRSSB
00958          MOVE SSB-CONT-TERMN-DATE-CEN (WS-SUB) TO SS-TERM-DATE    ELPPRSSB
00959      ELSE                                                         ELPPRSSB
00960          MOVE ALL '?'                TO SS-TERM-DATE-A            ELPPRSSB
00961          SET WS-INVALID-DATA-FOUND TO TRUE                        ELPPRSSB
00962      END-IF.                                                      ELPPRSSB
00963 /                                                                 ELPPRSSB
00964  9000-PRINT-LINE.                                                 ELPPRSSB
00965      SET PCB-PRINT-LINE             TO TRUE.                      ELPPRSSB
00966      INSPECT PCB-PRINT-AREA REPLACING ALL LOW-VALUE BY '_'.       ELPPRSSB
00967      CALL 'ELPPR' USING PCB-PRINT-CONTROL-BLOCK.                  ELPPRSSB
00968                                                                   ELPPRSSB
00969  9010-CONVERT-DATE.                                               ELPPRSSB
00970      CALL 'TSGGREG' USING WS-JULIAN-DATE WS-GREGORIAN-DATE.       ELPPRSSB
00971 /                                                                 ELPPRSSB
00972      COPY ELSTCOMP.                                               ELPPRSSB
