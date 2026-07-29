00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGABM  
00003  PROGRAM-ID.           ELGABM.                                       LV002
00004                                                                   ELGABM  
00005  AUTHOR.               LUCY TORRES.                               ELGABM  
00006                        RICHARD J. LUKETICH.                       ELGABM  
00007                                                                   ELGABM  
00008                                                                   ELGABM  
00009                                                                   ELGABM  
00010  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELGABM  
00011                        A MUTUAL LEGAL RESERVE COMPANY             ELGABM  
00012                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELGABM  
00013                        233 N. MICHIGAN AVE                        ELGABM  
00014                        CHICAGO, ILLINOIS 60601                    ELGABM  
00015                                                                   ELGABM  
00016  DATE-WRITTEN.         15-JUN-1987.                               ELGABM  
00017                        27-DEC-1991  (REWRITE, R. LUKETICH).       ELGABM  
00018                                                                   ELGABM  
00019  DATE-COMPILED.                                                   ELGABM  
00020                                                                   ELGABM  
00021  SECURITY.             COPYRIGHT 1986, 1991                       ELGABM  
00022                        HEALTH CARE SERVICE CORPORATION            ELGABM  
00023      SKIP3                                                        ELGABM  
00024  ENVIRONMENT DIVISION.                                            ELGABM  
00025                                                                   ELGABM  
00026  CONFIGURATION SECTION.                                           ELGABM  
00027  SOURCE-COMPUTER.      IBM-3090.                                  ELGABM  
00028  OBJECT-COMPUTER.      IBM-3090.                                  ELGABM  
00029 /*****************************************************************ELGABM  
00030 *                                                                *ELGABM  
00031 *  ELGABM   - ELS:  GENERATES THE OUTPUT FOR MAXIMUMS AT THE     *ELGABM  
00032 *                   TOPIC, BENEFIT PROVISION, AND COST           *ELGABM  
00033 *                   CONTAINMENT LEVELS.                          *ELGABM  
00034 *                                                                *ELGABM  
00035 ******************************************************************ELGABM  
00036 *                                                                *ELGABM  
00037 *                      MAINTENANCE HISTORY                       *ELGABM  
00038 *                                                                *ELGABM  
00039 *  MOD     DATE     BY  DRPT                ACTION               *ELGABM  
00040 * ----- ----------- --- ----- ---------------------------------- *ELGABM  
00041 * 01.00 19-JUN-1987 LET       CREATED                            *ELGABM  
00042 *                                                                *ELGABM  
00043 * 01.01 18-AUG-1987 LET       CREATING THE VALUE LIMIT SENTENCE  *ELGABM  
00044 *                             BEFORE THE APPLICABILITY SENTENCE. *ELGABM  
00045 *                                                                *ELGABM  
00046 * 01.02 14-SEP-1987 REB       ADDED THE DEFINITION SENTENCE FOR  *ELGABM  
00047 *                             THE TOPIC AND BP LEVELS.           *ELGABM  
00048 *                                                                *ELGABM  
00049 * 01.03 19-SEP-1987 LET       ADDED CODE FOR THE GROUP SPECIFIC  *ELGABM  
00050 *                             AND CONTRACT LEVEL ACCUMS.         *ELGABM  
00051 *                                                                *ELGABM  
00052 * 01.04 29-SEP-1987 REB       INSERTED 88 LEVEL FOR WS-VALUE-LMT-YELGABM  
00053 *                             ALSO NEEDED TO CHANGE CONDITIONALS *ELGABM  
00054 *                             TO ACCOMODATE THE G/C ACCUMS.      *ELGABM  
00055 *                                                                *ELGABM  
00056 * 01.05 29-SEP-1987 REB       SEPERATE COST CONTAINMENT LEVEL    *ELGABM  
00057 *                             PROCESSING FROM THE OTHER LEVELS,  *ELGABM  
00058 *                             COMBINING THE VALUE LIMIT AND      *ELGABM  
00059 *                             APPLICABILITY SENTENCES FOR CC LVL.*ELGABM  
00060 *                                                                *ELGABM  
00061 * 01.06 01-OCT-1987 REB       THE CC LEVEL WILL CHECK THE VALUE  *ELGABM  
00062 *                             LIMIT < 0 AND HANDLE IT LIKE TOPIC.*ELGABM  
00063 *                                                                *ELGABM  
00064 * 01.07 09-OCT-1987 LET       DELETED CODE FOR GROUP SPECIFIC &  *ELGABM  
00065 *                             CONTRACT LEVEL ACCUMS.             *ELGABM  
00066 *                                                                *ELGABM  
00067 * 01.08 17-NOV-1987 REB       MADE CHANGES TO CORRESPOND TO NEW  *ELGABM  
00068 *                             VERSION OF COPYBOOK ELSACUMC.      *ELGABM  
00069 *                                                                *ELGABM  
00070 * 01.09 10-MAY-1988 LET       MADE CHANGES FOR BENEFIT PROVISON  *ELGABM  
00071 *                             LEVEL.                             *ELGABM  
00072 *                                                                *ELGABM  
00073 * 01.10 12-DEC-1990 JPB       MADE CHANGES FOR NEW STORAGE       *ELGABM  
00074 *                             MANAGEMENT.                        *ELGABM  
00075 *                                                                *ELGABM  
00076 * 01.11    SEP-1991 RKH       ADD LOGIC TO DISPLAY THE NEW       *ELGABM  
00077 *                             PATIENT AGE AND RELATIONSHIP       *ELGABM  
00078 *                             INDICATOR VALUES.                  *ELGABM  
00079 *                                                                *ELGABM  
00080 * 02.00 27-DEC-1991 RJL       RESTRUCTURED PROGRAM.              *ELGABM  
00081 *                                                                *ELGABM  
00082 * 02.01 21-AUG-2000 AKK       ADDED SUPPORT FOR #IPGS            *ELGABM  
00083 *                                                                *ELGABM  
ED0624* BBDA-58217 06/14/24  ED     RECOMPILE FOR PEAQ COPYBOOK        *        
ED0624*                             EXPANSION:                         *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00084 ******************************************************************ELGABM  
00085      TITLE 'WORKING STORAGE SECTION'.                             ELGABM  
00086  DATA DIVISION.                                                   ELGABM  
00087  WORKING-STORAGE SECTION.                                         ELGABM  
00088                                                                   ELGABM  
00089 * -- HEADERS                                                      ELGABM  
00090                                                                   ELGABM  
00091   01  HD-TOPIC-HDR .                                              ELGABM  
00092       02                          PICTURE  X(35) VALUE SPACES.    ELGABM  
00093       02                          PICTURE  X(08) VALUE            ELGABM  
00094          'MAXIMUMS'.                                              ELGABM  
00095       02                          PICTURE  X(36) VALUE SPACES.    ELGABM  
00096                                                                   ELGABM  
00097   01  HD-BEN-PROVN-HDR.                                           ELGABM  
00098       02                          PICTURE  X(25) VALUE            ELGABM  
00099          'MAXIMUM AT BENEFIT LEVEL:'.                             ELGABM  
00100       02                          PICTURE  X(54) VALUE SPACES.    ELGABM  
00101                                                                   ELGABM  
00102 * -- PHRASES AND TERMS - CONSTANT                                 ELGABM  
00103                                                                   ELGABM  
00104  01  PH-APPLIC-1-LEAD            PICTURE  X(24) VALUE             ELGABM  
00105      'THIS MAXIMUM APPLIES PER'.                                  ELGABM  
00106                                                                   ELGABM  
00107  01  PH-APPLIC-2-LINK            PICTURE  X(18) VALUE             ELGABM  
00108      'MAXIMUM APPLIES TO'.                                        ELGABM  
00109                                                                   ELGABM  
00110  01  PH-DEFN-LEAD                PICTURE  X(32) VALUE             ELGABM  
00111      'DOLLARS ARE ACCUMULATED BASED ON'.                          ELGABM  
00112                                                                   ELGABM  
00113  01  PH-INTRNL-TAB-LEAD          PICTURE  X(12) VALUE             ELGABM  
00114      ', SUBJECT TO'.                                              ELGABM  
00115                                                                   ELGABM  
00116  01  PH-INTRNL-TAB-TRAIL         PICTURE  X(27) VALUE             ELGABM  
00117      'CONSIDERATIONS LISTED BELOW'.                               ELGABM  
00118                                                                   ELGABM  
00119  01  PH-INTRVL-OVRD-LEAD         PICTURE  X(06) VALUE             ELGABM  
00120      '(NOTE:'.                                                    ELGABM  
00121                                                                   ELGABM  
00122  01  PH-INTRVL-OVRD-TRAIL        PICTURE  X(02) VALUE             ELGABM  
00123      ').'.                                                        ELGABM  
00124                                                                   ELGABM  
00125  01  PH-INTRVL-OVRD-SPCL-1-LEAD  PICTURE  X(32) VALUE             ELGABM  
00126      'THE INTERVAL MAY BE OVERRULED IF'.                          ELGABM  
00127                                                                   ELGABM  
00128  01  PH-INTRVL-OVRD-SPCL-1-TRAIL PICTURE  X(75) VALUE             ELGABM  
00129      'MONTHS HAVE ELAPSED FROM THE ADMISSION DATE OF THE FIRST COVELGABM  
00130 -    'ERED ADMISSION.'.                                           ELGABM  
00131                                                                   ELGABM  
00132  01  PH-INTRVL-OVRD-SPCL-2-LEAD  PICTURE  X(72) VALUE             ELGABM  
00133      'IF THE MEMBER IS MEDICARE ELIGIBLE, THE BENEFIT PERIODS ARE ELGABM  
00134 -    'SEPARATED BY'.                                              ELGABM  
00135                                                                   ELGABM  
00136  01  PH-INTRVL-OVRD-SPCL-2-TRAIL PICTURE  X(05) VALUE             ELGABM  
00137      'DAYS.'.                                                     ELGABM  
00138                                                                   ELGABM  
00139  01  PH-INTRVL-OVRD-SPCL-3-LEAD  PICTURE  X(37) VALUE             ELGABM  
00140      'THE INTERVAL CAN BE OVERRULED SO THAT'.                     ELGABM  
00141                                                                   ELGABM  
00142  01  PH-INTRVL-OVRD-SPCL-3-TRAIL PICTURE  X(52) VALUE             ELGABM  
00143      'DAYS/VISITS ARE PAID AT THE INDICATED PERCENT LEVEL.'.      ELGABM  
00144                                                                   ELGABM  
00145  01  PH-MAX-VAL-SENT-LEAD         PICTURE  X(37) VALUE            ELGABM  
00146      'SERVICES ARE COVERED UP TO A MAXIMUM '.                     ELGABM  
00147                                                                   ELGABM  
00148  01  PH-MAX-OTHR-SRCE-LEAD        PICTURE  X(09) VALUE            ELGABM  
00149      'FOUND IN '.                                                 ELGABM  
00150                                                                   ELGABM  
00151  01  PH-MAX-OTHR-SRCE-UNKN        PICTURE  X(15) VALUE            ELGABM  
00152      'ANOTHER SOURCE'.                                            ELGABM  
00153                                                                   ELGABM  
00154  01  PH-NO-MAX                   PICTURE  X(42) VALUE             ELGABM  
00155      'NO GROUP OR CONTRACT LEVEL MAXIMUMS APPLY.'.                ELGABM  
00156                                                                   ELGABM  
00157  01  PH-NO-INST-MAX              PICTURE  X(56) VALUE             ELGABM  
00158      'NO INSTITUTIONAL GROUP OR CONTRACT LEVEL MAXIMUMS APPLY.'.  ELGABM  
00159                                                                   ELGABM  
00160  01  PH-NO-PROF-MAX              PICTURE  X(55) VALUE             ELGABM  
00161      'NO PROFESSIONAL GROUP OR CONTRACT LEVEL MAXIMUMS APPLY.'.   ELGABM  
00162                                                                   ELGABM  
00163  01  PH-NO-PT-RLTNSHP            PICTURE  X(11) VALUE             ELGABM  
00164      'TO PATIENTS'.                                               ELGABM  
00165                                                                   ELGABM  
00166  01  PH-PT-FROM-AGE              PICTURE  X(08) VALUE             ELGABM  
00167      'FROM AGE'.                                                  ELGABM  
00168                                                                   ELGABM  
00169  01  PH-PT-TO-AGE                PICTURE  X(06) VALUE             ELGABM  
00170      'TO AGE'.                                                    ELGABM  
00171                                                                   ELGABM  
00172  01  PH-REINST-LEAD              PICTURE  X(08) VALUE             ELGABM  
00173      ', AND IS'.                                                  ELGABM  
00174                                                                   ELGABM  
00175  01  PH-TIME-FCTR-LEAD           PICTURE  X(09) VALUE             ELGABM  
00176      'PERIOD OF'.                                                 ELGABM  
00177                                                                   ELGABM  
00178  01  PH-TIME-INTRVL-LEAD         PICTURE  X(12) VALUE             ELGABM  
00179      'SEPARATED BY'.                                              ELGABM  
00180                                                                   ELGABM  
00181  01  PH-SEE-BP-TOPICS-1          PICTURE  X(53) VALUE             ELGABM  
00182      'ADDITIONAL MAXIMUMS MAY APPLY TO INDIVIDUAL BENEFITS.'.     ELGABM  
00183                                                                   ELGABM  
00184  01  PH-SEE-BP-TOPICS-2          PICTURE  X(44) VALUE             ELGABM  
00185      'SEE SPECIFIC TOPICS FOR ADDITIONAL MAXIMUMS.'.              ELGABM  
00186                                                                   ELGABM  
00187  01  PH-SEE-MAX-TOPIC             PICTURE  X(47) VALUE            ELGABM  
00188      'SEE THE MAXIMUMS TOPIC FOR ADDITIONAL MAXIMUMS.'.           ELGABM  
00189                                                                   ELGABM  
00190 * -- PHRASES AND TERMS - SINGLE WORDS                             ELGABM  
00191                                                                   ELGABM  
00192  01  PH-WD-BENEFITS    PICTURE  X(10) VALUE 'BENEFITS '.          ELGABM  
00193  01  PH-WD-FOR         PICTURE  X(04) VALUE 'FOR '.               ELGABM  
00194  01  PH-WD-OF          PICTURE  X(03) VALUE 'OF '.                ELGABM  
00195  01  PH-WD-PER         PICTURE  X(04) VALUE 'PER '.               ELGABM  
00196  01  PH-WD-PATIENTS    PICTURE  X(09) VALUE 'PATIENTS '.          ELGABM  
00197  01  PH-WD-PROVIDED    PICTURE  X(09) VALUE 'PROVIDED '.          ELGABM  
00198  01  PH-WD-SERVICES    PICTURE  X(09) VALUE 'SERVICES '.          ELGABM  
00199  01  PH-WD-THE         PICTURE  X(04) VALUE 'THE '.               ELGABM  
00200  01  PH-WD-THIS        PICTURE  X(05) VALUE 'THIS '.              ELGABM  
00201  01  PH-WD-TO          PICTURE  X(03) VALUE 'TO '.                ELGABM  
00202  01  PH-WD-UNLIMITED   PICTURE  X(10) VALUE 'UNLIMITED '.         ELGABM  
00203                                                                   ELGABM  
00204 * -- PHRASES AND TERMS - NUMERIC FORMAT AREAS                     ELGABM  
00205                                                                   ELGABM  
00206  01  PH-AGE-LIM-FROM             PICTURE  ZZ9B.                   ELGABM  
00207                                                                   ELGABM  
00208  01  PH-AGE-LIM-TO               PICTURE  ZZ9B.                   ELGABM  
00209                                                                   ELGABM  
00210  01  PH-BEN-PER-TIME-FCTR        PICTURE  ZZ9B.                   ELGABM  
00211                                                                   ELGABM  
00212  01  PH-INTRVL-TIME-FCTR         PICTURE  ZZ9B.                   ELGABM  
00213                                                                   ELGABM  
00214  01  PH-INTRVL-OVRD-VAL          PICTURE  ZZZZ9B.                 ELGABM  
00215                                                                   ELGABM  
00216  01  PH-MAX-VAL-LMT-DOLLARS      PICTURE  $$,$$$,$$9.99B.         ELGABM  
00217                                                                   ELGABM  
00218  01  PH-MAX-VAL-LMT-OTHER        PICTURE  ZZZ,ZZZ,Z99B.           ELGABM  
00219                                                                   ELGABM  
00220 * -- OTHERS                                                       ELGABM  
00221                                                                   ELGABM  
00222  01  PROGRAM-CONSTANTS.                                           ELGABM  
00223      05  PC-ABM                      PIC  X(06) VALUE             ELGABM  
00224              '#ABM  '.                                            ELGABM  
00225      05  PC-IBGR                     PIC  X(06) VALUE             ELGABM  
00226              '#IBGR '.                                            ELGABM  
00227      05  PC-IDGD                     PIC  X(06) VALUE             ELGABM  
00228              '#IDGD '.                                            ELGABM  
00229      05  PC-IPGN                     PIC  X(06) VALUE             ELGABM  
00230              '#IPGN '.                                            ELGABM  
00231      05  PC-IPGP                     PIC  X(06) VALUE             ELGABM  
00232              '#IPGP '.                                            ELGABM  
00233      05  PC-IPGT                     PIC  X(06) VALUE             ELGABM  
00234              '#IPGT '.                                            ELGABM  
00235      05  PC-IPGS                     PIC  X(06) VALUE             ELGABM  
00236              '#IPGS '.                                            ELGABM  
00237                                                                   ELGABM  
00238 / -- INTERNAL TABULAR INFORMATION WORK AREAS                      ELGABM  
00239 * -- INTERNAL TABULAR SWITCHES                                    ELGABM  
00240                                                                   ELGABM  
00241  01  WS-INT-TAB-SW.                                               ELGABM  
00242      02                          PICTURE  X(01).                  ELGABM  
00243         88 SW-HAS-INTERNALS      VALUE 'Y'.                       ELGABM  
00244         88 SW-HAS-NO-INTERNALS   VALUE 'N'.                       ELGABM  
00245      02                          PICTURE  X(01).                  ELGABM  
00246         88 SW-HAS-IBGR           VALUE 'Y'.                       ELGABM  
00247         88 SW-HAS-NO-IBGR        VALUE 'N'.                       ELGABM  
00248      02                          PICTURE  X(01).                  ELGABM  
00249         88 SW-HAS-IDGD           VALUE 'Y'.                       ELGABM  
00250         88 SW-HAS-NO-IDGD        VALUE 'N'.                       ELGABM  
00251      02                          PICTURE  X(01).                  ELGABM  
00252         88 SW-HAS-IPGN           VALUE 'Y'.                       ELGABM  
00253         88 SW-HAS-NO-IPGN        VALUE 'N'.                       ELGABM  
00254      02                          PICTURE  X(01).                  ELGABM  
00255         88 SW-HAS-IPGP           VALUE 'Y'.                       ELGABM  
00256         88 SW-HAS-NO-IPGP        VALUE 'N'.                       ELGABM  
00257      02                          PICTURE  X(01).                  ELGABM  
00258         88 SW-HAS-IPGT           VALUE 'Y'.                       ELGABM  
00259         88 SW-HAS-NO-IPGT        VALUE 'N'.                       ELGABM  
00260      02                          PICTURE  X(01).                  ELGABM  
00261         88 SW-HAS-IPGS           VALUE 'Y'.                       ELGABM  
00262         88 SW-HAS-NO-IPGS        VALUE 'N'.                       ELGABM  
00263                                                                   ELGABM  
00264  01  WS-INT-TAB-LST.                                              ELGABM  
00265      02 WS-TAB-SUB                PIC S9(04) COMP.                ELGABM  
00266      02 WS-INT-TAB                OCCURS 6 TIMES.                 ELGABM  
00267         03                        PIC  X(18).                     ELGABM  
00268            88 WS-INT-TAB-IBGR VALUE ' BENEFIT PROVISION'.         ELGABM  
00269            88 WS-INT-TAB-IDGD VALUE '         DIAGNOSIS'.         ELGABM  
00270            88 WS-INT-TAB-IPGN VALUE '   PROVIDER NUMBER'.         ELGABM  
00271            88 WS-INT-TAB-IPGP VALUE '         PROCEDURE'.         ELGABM  
00272            88 WS-INT-TAB-IPGT VALUE '     PROVIDER TYPE'.         ELGABM  
00273            88 WS-INT-TAB-IPGS VALUE 'PROVIDER SPECIALTY'.         ELGABM  
00274         03                        PIC  X(05).                     ELGABM  
00275            88 WS-INT-TAB-AND      VALUE ' AND '.                  ELGABM  
00276            88 WS-INT-TAB-COMMA    VALUE ',    '.                  ELGABM  
00277            88 WS-INT-TAB-END      VALUE SPACES.                   ELGABM  
00278                                                                   ELGABM  
00279  01  WS-INT-TAB-TXT               REDEFINES WS-INT-TAB-LST.       ELGABM  
00280      02                           PIC S9(04) COMP.                ELGABM  
00281      02 WS-INT-TAB-TXT-1          PICTURE  X(69).                 ELGABM  
00282      02 WS-INT-TAB-TXT-2          PICTURE  X(46).                 ELGABM  
00283                                                                   ELGABM  
00284  01  WS-TEXT-HOLD-AREA.                                           ELGABM  
00285      02 WS-TEXT-HOLD-COUNT       PICTURE S9(4)           COMP.    ELGABM  
00286      02 WS-TEXT-HOLD-TEXT        PICTURE  X(1580).                ELGABM  
00287      TITLE 'LINKAGE SECTION'.                                     ELGABM  
00288  LINKAGE SECTION.                                                 ELGABM  
00289                                                                   ELGABM  
00290  01  DFHCOMMAREA.                                                 ELGABM  
00291      COPY ELSCOMMC.                                               ELGABM  
00292 /                                                                 ELGABM  
00293      COPY ELSCIA2C.                                               ELGABM  
00294 /                                                                 ELGABM  
00295      COPY ELSIOPMC.                                               ELGABM  
00296 /                                                                 ELGABM  
00297      COPY ELSCMIFC.                                               ELGABM  
00298 /                                                                 ELGABM  
00299      COPY ELSCMDSC.                                               ELGABM  
00300 /                                                                 ELGABM  
00301      COPY ELSOUTPC.                                               ELGABM  
00302 /                                                                 ELGABM  
00303      COPY ELSSRTPC.                                               ELGABM  
00304 /                                                                 ELGABM  
00305      COPY ELSSSCBC.                                               ELGABM  
00306 /                                                                 ELGABM  
00307      COPY ELSTCWAC.                                               ELGABM  
00308 /                                                                 ELGABM  
00309      COPY ELSACUMC.                                               ELGABM  
00310      TITLE 'PROCEDURE DIVISION'.                                  ELGABM  
00311 ************************************************************      ELGABM  
00312 *                                                          *      ELGABM  
00313 *    PROCEDURE DIVISION                                    *      ELGABM  
00314 *                                                          *      ELGABM  
00315 ************************************************************      ELGABM  
00316                                                                   ELGABM  
00317  PROCEDURE DIVISION.                                              ELGABM  
00318                                                                   ELGABM  
00319      PERFORM 0010-INITIALIZATION.                                 ELGABM  
00320      PERFORM 0130-PROCESS.                                        ELGABM  
00321      GOBACK.                                                      ELGABM  
00322                                                                   ELGABM  
00323 /***********************************************************      ELGABM  
00324 *                                                          *      ELGABM  
00325 *        INITIALIZATION                                    *      ELGABM  
00326 *                                                          *      ELGABM  
00327 ************************************************************      ELGABM  
00328                                                                   ELGABM  
00329  0010-INITIALIZATION.                                             ELGABM  
00330 * -- ESTABLISH STANDARD ENVIRONMENT                               ELGABM  
00331      PERFORM 0020-EST-ADR-OF-CONTROL-BLOCKS.                      ELGABM  
00332                                                                   ELGABM  
00333 * -- ESTABLISH ADDRESSABILITY OF WORK AREAS                       ELGABM  
00334      PERFORM 0070-EST-ADR-OF-TEMPORARY-FILE.                      ELGABM  
00335      PERFORM 0080-EST-ADR-OF-CDES-MANUAL.                         ELGABM  
00336      PERFORM 0090-EST-ADR-OF-OUTPUT-INTERFA.                      ELGABM  
00337      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGABM  
00338      PERFORM 0110-EST-ADR-OF-TEXT-COMPRESSX.                      ELGABM  
00339                                                                   ELGABM  
00340 * -- CLEAR OUTPUT AND INITIALIZE TEXT COMPRESSION WORK AREA       ELGABM  
00341      IF COF-NBR-DTL-LINES > 0                                     ELGABM  
00342      THEN                                                         ELGABM  
00343         CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.      ELGABM  
00344                                                                   ELGABM  
00345      INITIALIZE TCAR-FROM-AREA                                    ELGABM  
00346                 TCAR-FROM-LENGTH                                  ELGABM  
00347                 TCAR-FROM-SUB.                                    ELGABM  
00348                                                                   ELGABM  
00349 ************************************************************      ELGABM  
00350 *                                                          *      ELGABM  
00351 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGABM  
00352 *                                                          *      ELGABM  
00353 ************************************************************      ELGABM  
00354                                                                   ELGABM  
00355  0020-EST-ADR-OF-CONTROL-BLOCKS.                                  ELGABM  
00356      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGABM  
00357      THEN                                                         ELGABM  
00358         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGABM  
00359      ELSE                                                         ELGABM  
00360         IF ECA-CIA-PTR = NULL                                     ELGABM  
00361         THEN                                                      ELGABM  
00362            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGABM  
00363         ELSE                                                      ELGABM  
00364            CALL 'ELUINISM'                                        ELGABM  
00365               USING DFHCOMMAREA                                   ELGABM  
00366                     ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA      ELGABM  
00367            SET CIA-ELSSSCB-DDN TO TRUE                            ELGABM  
00368            CALL 'ELUSETAD'                                        ELGABM  
00369               USING DFHCOMMAREA                                   ELGABM  
00370                     ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK        ELGABM  
00371            IF CIA-RC-PTR-NULL                                     ELGABM  
00372            THEN                                                   ELGABM  
00373               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGABM  
00374               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGABM  
00375            ELSE                                                   ELGABM  
00376               CONTINUE                                            ELGABM  
00377            END-IF                                                 ELGABM  
00378         END-IF                                                    ELGABM  
00379      END-IF.                                                      ELGABM  
00380                                                                   ELGABM  
00381 /***********************************************************      ELGABM  
00382 *                                                          *      ELGABM  
00383 *        ESTABLISH ADDRESSABILITY OF TEMPORARY FILE        *      ELGABM  
00384 *                                                          *      ELGABM  
00385 ************************************************************      ELGABM  
00386                                                                   ELGABM  
00387  0070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGABM  
00388      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGABM  
00389      CALL 'ELUSETAD'                                              ELGABM  
00390         USING DFHCOMMAREA                                         ELGABM  
00391               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELGABM  
00392      IF CIA-RC-PTR-NULL                                           ELGABM  
00393         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABM  
00394         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGABM  
00395                                                                   ELGABM  
00396 ************************************************************      ELGABM  
00397 *                                                          *      ELGABM  
00398 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE*      ELGABM  
00399 *                                                          *      ELGABM  
00400 ************************************************************      ELGABM  
00401                                                                   ELGABM  
00402  0080-EST-ADR-OF-CDES-MANUAL.                                     ELGABM  
00403      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGABM  
00404      CALL 'ELUSETAD'                                              ELGABM  
00405         USING DFHCOMMAREA                                         ELGABM  
00406               ADDRESS OF CMF-CODES-MANUAL-INTERFACE.              ELGABM  
00407      IF CIA-RC-PTR-NULL                                           ELGABM  
00408         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABM  
00409         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGABM  
00410                                                                   ELGABM  
00411 ************************************************************      ELGABM  
00412 *                                                          *      ELGABM  
00413 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGABM  
00414 *                                                          *      ELGABM  
00415 ************************************************************      ELGABM  
00416                                                                   ELGABM  
00417  0090-EST-ADR-OF-OUTPUT-INTERFA.                                  ELGABM  
00418      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGABM  
00419      CALL 'ELUSETAD'                                              ELGABM  
00420         USING DFHCOMMAREA                                         ELGABM  
00421               ADDRESS OF COF-OUTPUT-INTERFACE.                    ELGABM  
00422      IF CIA-RC-PTR-NULL                                           ELGABM  
00423         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABM  
00424         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGABM  
00425                                                                   ELGABM  
00426 ************************************************************      ELGABM  
00427 *                                                          *      ELGABM  
00428 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGABM  
00429 *                                                          *      ELGABM  
00430 ************************************************************      ELGABM  
00431                                                                   ELGABM  
00432  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGABM  
00433      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGABM  
00434      CALL 'ELUSETAD'                                              ELGABM  
00435         USING DFHCOMMAREA                                         ELGABM  
00436               ADDRESS OF SRP-SUBROUTINE-PARAMETERS.               ELGABM  
00437      IF CIA-RC-PTR-NULL                                           ELGABM  
00438         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABM  
00439         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGABM  
00440                                                                   ELGABM  
00441 ************************************************************      ELGABM  
00442 *                                                          *      ELGABM  
00443 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELGABM  
00444 *                                                          *      ELGABM  
00445 ************************************************************      ELGABM  
00446                                                                   ELGABM  
00447  0110-EST-ADR-OF-TEXT-COMPRESSX.                                  ELGABM  
00448      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGABM  
00449      CALL 'ELUSETAD'                                              ELGABM  
00450         USING DFHCOMMAREA                                         ELGABM  
00451               ADDRESS OF TCAR-COMPRESSION-WORK-AREA.              ELGABM  
00452      IF CIA-RC-PTR-NULL                                           ELGABM  
00453         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGABM  
00454         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGABM  
00455                                                                   ELGABM  
00456 /***********************************************************      ELGABM  
00457 *                                                          *      ELGABM  
00458 *        PROCESS                                           *      ELGABM  
00459 *                                                          *      ELGABM  
00460 ************************************************************      ELGABM  
00461                                                                   ELGABM  
00462  0130-PROCESS.                                                    ELGABM  
00463 * -- GENERATE INITIAL HEADER                                      ELGABM  
00464      EVALUATE TRUE                                                ELGABM  
00465          WHEN SRP-TOPIC-ACCUM                                     ELGABM  
00466               PERFORM 0140-OBTAIN-TOPIC-HEADER                    ELGABM  
00467          WHEN SRP-BEN-PROV-ACCUM                                  ELGABM  
00468               PERFORM 0150-OBTAIN-BEN-PROVN-HEADER                ELGABM  
00469      END-EVALUATE.                                                ELGABM  
00470                                                                   ELGABM  
00471 * -- GENERATE ACCUMULATOR OUTPUT                                  ELGABM  
00472      EVALUATE TRUE                                                ELGABM  
00473         WHEN SRP-NO-ACCUMS-FOUND                                  ELGABM  
00474            PERFORM 0170-DSPLY-NO-ACCUMS                           ELGABM  
00475         WHEN SRP-INST-NOT-APPLICABLE                              ELGABM  
00476            PERFORM 0180-DSPLY-INST-NOT-APPLIC                     ELGABM  
00477         WHEN SRP-PROF-NOT-APPLICABLE                              ELGABM  
00478            PERFORM 0190-DSPLY-PROF-NOT-APPLIC                     ELGABM  
00479         WHEN OTHER                                                ELGABM  
00480            PERFORM 0200-DISPLAY-REGULAR-TEXT                      ELGABM  
00481      END-EVALUATE.                                                ELGABM  
00482      IF SRP-TOPIC-ACCUM                                           ELGABM  
00483         PERFORM 0680-END-THE-DISPLAY.                             ELGABM  
00484                                                                   ELGABM  
00485 /***********************************************************      ELGABM  
00486 *                                                          *      ELGABM  
00487 *        OBTAIN TOPIC HEADER                               *      ELGABM  
00488 *                                                          *      ELGABM  
00489 ************************************************************      ELGABM  
00490                                                                   ELGABM  
00491  0140-OBTAIN-TOPIC-HEADER.                                        ELGABM  
00492      MOVE +2            TO  COF-NBR-HDR-LINES.                    ELGABM  
00493      MOVE HD-TOPIC-HDR  TO  COF-HDR-LINE (2).                     ELGABM  
00494      MOVE  0            TO  COF-NBR-DTL-LINES.                    ELGABM  
00495      SET  COF-NEW-PAGE  TO  TRUE.                                 ELGABM  
00496      PERFORM 0160-SEND-INITL-HDR.                                 ELGABM  
00497                                                                   ELGABM  
00498 ************************************************************      ELGABM  
00499 *                                                          *      ELGABM  
00500 *        OBTAIN BENEFIT PROVISION HEADER                   *      ELGABM  
00501 *                                                          *      ELGABM  
00502 ************************************************************      ELGABM  
00503                                                                   ELGABM  
00504  0150-OBTAIN-BEN-PROVN-HEADER.                                    ELGABM  
00505      INITIALIZE COF-DTL-LINE (1).                                 ELGABM  
00506      MOVE HD-BEN-PROVN-HDR TO COF-DTL-LINE (2).                   ELGABM  
00507      SET  COF-CONTINUE  TO  TRUE.                                 ELGABM  
00508      MOVE +0            TO  COF-NBR-HDR-LINES.                    ELGABM  
00509      MOVE +2            TO  COF-NBR-DTL-LINES.                    ELGABM  
00510      PERFORM 0160-SEND-INITL-HDR.                                 ELGABM  
00511                                                                   ELGABM  
00512 ************************************************************      ELGABM  
00513 *                                                          *      ELGABM  
00514 *    SEND INITIAL HEADER                                   *      ELGABM  
00515 *                                                          *      ELGABM  
00516 ************************************************************      ELGABM  
00517                                                                   ELGABM  
00518  0160-SEND-INITL-HDR.                                             ELGABM  
00519      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGABM  
00520      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGABM  
00521      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABM  
00522      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGABM  
00523                                                                   ELGABM  
00524 /***********************************************************      ELGABM  
00525 *                                                          *      ELGABM  
00526 *        DISPLAY NO ACCUMS MESSAGE                         *      ELGABM  
00527 *                                                          *      ELGABM  
00528 ************************************************************      ELGABM  
00529                                                                   ELGABM  
00530  0170-DSPLY-NO-ACCUMS.                                            ELGABM  
00531      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00532      MOVE PH-NO-MAX TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGABM  
00533      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGABM  
00534                                                                   ELGABM  
00535 ************************************************************      ELGABM  
00536 *                                                          *      ELGABM  
00537 *    DISPLAY INSTITUTIONAL MAXIMUMS NOT APPLICABLE         *      ELGABM  
00538 *                                                          *      ELGABM  
00539 ************************************************************      ELGABM  
00540                                                                   ELGABM  
00541  0180-DSPLY-INST-NOT-APPLIC.                                      ELGABM  
00542      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00543      MOVE PH-NO-INST-MAX TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGABM  
00544      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGABM  
00545                                                                   ELGABM  
00546 ************************************************************      ELGABM  
00547 *                                                          *      ELGABM  
00548 *    DISPLAY PROFESSIONAL MAXIMUMS NOT APPLICABLE          *      ELGABM  
00549 *                                                          *      ELGABM  
00550 ************************************************************      ELGABM  
00551                                                                   ELGABM  
00552  0190-DSPLY-PROF-NOT-APPLIC.                                      ELGABM  
00553      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00554      MOVE PH-NO-PROF-MAX TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGABM  
00555      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGABM  
00556                                                                   ELGABM  
00557 /***********************************************************      ELGABM  
00558 *                                                          *      ELGABM  
00559 *        DISPLAY REGULAR TEXT                              *      ELGABM  
00560 *                                                          *      ELGABM  
00561 ************************************************************      ELGABM  
00562                                                                   ELGABM  
00563  0200-DISPLAY-REGULAR-TEXT.                                       ELGABM  
00564 * -- DO INITIAL READ                                              ELGABM  
00565      MOVE 1 TO IOP-TSQ-ITEM-NBR.                                  ELGABM  
00566      SET  IOP-RD  TO  TRUE.                                       ELGABM  
00567      PERFORM 0660-READ-ACCUM-WORK-FILE-REC.                       ELGABM  
00568                                                                   ELGABM  
00569 * -- PROCESS UNTIL DONE                                           ELGABM  
00570      PERFORM 0210-DISPLAY-OCCURENCE-TEXT                          ELGABM  
00571          UNTIL NOT IOP-RC-OK.                                     ELGABM  
00572                                                                   ELGABM  
00573 * -- DELETE WORK FILE WHEN DONE                                   ELGABM  
00574      PERFORM 0670-DELETE-ACCUM-WORK-FILE.                         ELGABM  
00575                                                                   ELGABM  
00576 /***********************************************************      ELGABM  
00577 *                                                          *      ELGABM  
00578 *        DISPLAY OCCURENCE TEXT                            *      ELGABM  
00579 *                                                          *      ELGABM  
00580 ************************************************************      ELGABM  
00581                                                                   ELGABM  
00582  0210-DISPLAY-OCCURENCE-TEXT.                                     ELGABM  
00583                                                                   ELGABM  
00584      PERFORM 0220-INIT-OCCRNC-PROC.                               ELGABM  
00585                                                                   ELGABM  
00586      PERFORM 0230-DISPLAY-COMMON-BODY-TEXT.                       ELGABM  
00587                                                                   ELGABM  
00588      SET IOP-RD-NXT  TO  TRUE.                                    ELGABM  
00589      PERFORM 0660-READ-ACCUM-WORK-FILE-REC.                       ELGABM  
00590                                                                   ELGABM  
00591      EVALUATE TRUE                                                ELGABM  
00592          WHEN SRP-TOPIC-ACCUM                                     ELGABM  
00593               PERFORM 0520-GEN-TOPIC-TRAILER                      ELGABM  
00594               IF IOP-RC-OK                                        ELGABM  
00595                  THEN                                             ELGABM  
00596                     PERFORM 0140-OBTAIN-TOPIC-HEADER              ELGABM  
00597                  ELSE                                             ELGABM  
00598                     CONTINUE                                      ELGABM  
00599               END-IF                                              ELGABM  
00600          WHEN SRP-BEN-PROV-ACCUM                                  ELGABM  
00601               PERFORM 0530-GEN-BP-TRAILER                         ELGABM  
00602      END-EVALUATE.                                                ELGABM  
00603                                                                   ELGABM  
00604 ************************************************************      ELGABM  
00605 *                                                          *      ELGABM  
00606 *    INITIALIZE OCCURRENCE PROCESSING                      *      ELGABM  
00607 *                                                          *      ELGABM  
00608 ************************************************************      ELGABM  
00609  0220-INIT-OCCRNC-PROC.                                           ELGABM  
00610      SET SW-HAS-NO-INTERNALS                                      ELGABM  
00611          SW-HAS-NO-IBGR                                           ELGABM  
00612          SW-HAS-NO-IDGD                                           ELGABM  
00613          SW-HAS-NO-IPGN                                           ELGABM  
00614          SW-HAS-NO-IPGP                                           ELGABM  
00615          SW-HAS-NO-IPGT                                           ELGABM  
00616          SW-HAS-NO-IPGS                                           ELGABM  
00617       TO TRUE.                                                    ELGABM  
00618                                                                   ELGABM  
00619 /***********************************************************      ELGABM  
00620 *                                                          *      ELGABM  
00621 *        DISPLAY COMMON BODY TEXT                          *      ELGABM  
00622 *                                                          *      ELGABM  
00623 ************************************************************      ELGABM  
00624                                                                   ELGABM  
00625  0230-DISPLAY-COMMON-BODY-TEXT.                                   ELGABM  
00626                                                                   ELGABM  
00627      PERFORM 0240-CHK-INTRNL-TABS.                                ELGABM  
00628                                                                   ELGABM  
00629      PERFORM 0250-GEN-MAX-VALUE-SENT.                             ELGABM  
00630                                                                   ELGABM  
00631      PERFORM 0350-GEN-APPLIC-SENT-1.                              ELGABM  
00632                                                                   ELGABM  
00633      PERFORM 0370-GEN-APPLIC-SENT-2.                              ELGABM  
00634                                                                   ELGABM  
00635      IF DEFINITION-NA                                             ELGABM  
00636      THEN                                                         ELGABM  
00637         CONTINUE                                                  ELGABM  
00638      ELSE                                                         ELGABM  
00639         PERFORM 0510-GEN-DEFN-SENT.                               ELGABM  
00640                                                                   ELGABM  
00641      SET ASC-DES-INDEX TO 1.                                      ELGABM  
00642      PERFORM 0650-GEN-INTRNL-TAB-LSTNGS.                          ELGABM  
00643                                                                   ELGABM  
00644 /***********************************************************      ELGABM  
00645 *                                                          *      ELGABM  
00646 *    CHECK FOR INTERNAL TABULARS                           *      ELGABM  
00647 *                                                          *      ELGABM  
00648 ************************************************************      ELGABM  
00649  0240-CHK-INTRNL-TABS.                                            ELGABM  
00650      IF NO-IBGR-SLOT-NBR (1)                                      ELGABM  
00651      THEN CONTINUE                                                ELGABM  
00652      ELSE SET SW-HAS-IBGR SW-HAS-INTERNALS TO TRUE.               ELGABM  
00653                                                                   ELGABM  
00654      IF NO-IDGD-SLOT-NBR (1)                                      ELGABM  
00655      THEN CONTINUE                                                ELGABM  
00656      ELSE SET SW-HAS-IDGD SW-HAS-INTERNALS TO TRUE.               ELGABM  
00657                                                                   ELGABM  
00658      IF NO-IPGN-SLOT-NBR (1)                                      ELGABM  
00659      THEN CONTINUE                                                ELGABM  
00660      ELSE SET SW-HAS-IPGN SW-HAS-INTERNALS TO TRUE.               ELGABM  
00661                                                                   ELGABM  
00662      IF NO-IPGP-SLOT-NBR (1)                                      ELGABM  
00663      THEN CONTINUE                                                ELGABM  
00664      ELSE SET SW-HAS-IPGP SW-HAS-INTERNALS TO TRUE.               ELGABM  
00665                                                                   ELGABM  
00666      IF NO-IPGT-SLOT-NBR (1)                                      ELGABM  
00667      THEN CONTINUE                                                ELGABM  
00668      ELSE SET SW-HAS-IPGT SW-HAS-INTERNALS TO TRUE.               ELGABM  
00669                                                                   ELGABM  
00670      IF NO-IPGS-SLOT-NBR (1)                                      ELGABM  
00671      THEN CONTINUE                                                ELGABM  
00672      ELSE SET SW-HAS-IPGS SW-HAS-INTERNALS TO TRUE.               ELGABM  
00673                                                                   ELGABM  
00674 /***********************************************************      ELGABM  
00675 *                                                          *      ELGABM  
00676 *        GENERATE MAXIMUM DESCRIPTION                      *      ELGABM  
00677 *                                                          *      ELGABM  
00678 ************************************************************      ELGABM  
00679  0250-GEN-MAX-VALUE-SENT.                                         ELGABM  
00680                                                                   ELGABM  
00681      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00682      MOVE PH-MAX-VAL-SENT-LEAD TO TCAR-FROM-LINE(TCAR-FROM-SUB).  ELGABM  
00683                                                                   ELGABM  
00684      IF ACCUM-VALUE-LIMIT (1) < ZERO                              ELGABM  
00685      THEN                                                         ELGABM  
00686         PERFORM 0260-DISPLAY-MAXIMUM-ELSEWHERE                    ELGABM  
00687      ELSE                                                         ELGABM  
00688         PERFORM 0270-GEN-STD-MAX-PH                               ELGABM  
00689      END-IF.                                                      ELGABM  
00690                                                                   ELGABM  
00691      IF BENEFIT-PERIOD-NA                                         ELGABM  
00692      THEN                                                         ELGABM  
00693         CONTINUE                                                  ELGABM  
00694      ELSE                                                         ELGABM  
00695         PERFORM 0310-GEN-BEN-PER-PH.                              ELGABM  
00696                                                                   ELGABM  
00697      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00698      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGABM  
00699                                                                   ELGABM  
00700      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGABM  
00701                                                                   ELGABM  
00702 /***********************************************************      ELGABM  
00703 *                                                          *      ELGABM  
00704 *        DISPLAY MAXIMUM ELSEWHERE MESSAGE                 *      ELGABM  
00705 *                                                          *      ELGABM  
00706 ************************************************************      ELGABM  
00707  0260-DISPLAY-MAXIMUM-ELSEWHERE.                                  ELGABM  
00708      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00709      MOVE PH-MAX-OTHR-SRCE-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).ELGABM  
00710                                                                   ELGABM  
00711      IF ACCUM-MAX-BASE-AMT-SOURCE-IND NOT = ZERO                  ELGABM  
00712      THEN                                                         ELGABM  
00713         ADD 1 TO TCAR-FROM-SUB                                    ELGABM  
00714         MOVE PH-WD-THE TO TCAR-FROM-LINE (TCAR-FROM-SUB)          ELGABM  
00715         MOVE ACCUM-MAX-BASE-AMT-SOURCE-IND TO CMF-CODE-VALUE      ELGABM  
00716         MOVE 'MAX-BASE-AMT-SOURC-IND' TO CMF-ELEMENT-SYSTEM-NAME  ELGABM  
00717         MOVE 'GROUP' TO CMF-RECORD-PREFIX                         ELGABM  
00718         EXEC CICS LINK PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)   ELGABM  
00719            END-EXEC                                               ELGABM  
00720         SET CIA-ELSCMDSC-DDN TO TRUE                              ELGABM  
00721         CALL 'ELUSETAD' USING DFHCOMMAREA ADDRESS OF CMF-DESCR    ELGABM  
00722         PERFORM 0550-MOVE-TRANSLATION-TO-COMPR                    ELGABM  
00723      ELSE                                                         ELGABM  
00724         ADD 1 TO TCAR-FROM-SUB                                    ELGABM  
00725         MOVE PH-MAX-OTHR-SRCE-UNKN                                ELGABM  
00726           TO TCAR-FROM-LINE (TCAR-FROM-SUB)                       ELGABM  
00727      END-IF.                                                      ELGABM  
00728                                                                   ELGABM  
00729      PERFORM 0600-SEND-PART-PARA.                                 ELGABM  
00730                                                                   ELGABM  
00731 /***********************************************************      ELGABM  
00732 *                                                          *      ELGABM  
00733 *    GENERATE STANDARD MAXIMUM PHRASE                      *      ELGABM  
00734 *                                                          *      ELGABM  
00735 ************************************************************      ELGABM  
00736  0270-GEN-STD-MAX-PH.                                             ELGABM  
00737      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00738      MOVE PH-WD-OF TO TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGABM  
00739                                                                   ELGABM  
00740      IF ACCUM-VAL-UNLIM (1)                                       ELGABM  
00741      THEN                                                         ELGABM  
00742         PERFORM 0280-GEN-UNLIM-TRM                                ELGABM  
00743      ELSE                                                         ELGABM  
00744         IF ACCUM-VALUE-QUALIFIER = '5'                            ELGABM  
00745         THEN                                                      ELGABM  
00746            PERFORM 0290-GEN-DOLLAR-LIM-TRM                        ELGABM  
00747         ELSE                                                      ELGABM  
00748            PERFORM 0300-GEN-OTHER-LIM-TRM                         ELGABM  
00749         END-IF                                                    ELGABM  
00750      END-IF.                                                      ELGABM  
00751                                                                   ELGABM  
00752 ************************************************************      ELGABM  
00753 *                                                          *      ELGABM  
00754 *    GENERATE UNLIMITED MAXIMUM TERM                       *      ELGABM  
00755 *                                                          *      ELGABM  
00756 ************************************************************      ELGABM  
00757  0280-GEN-UNLIM-TRM.                                              ELGABM  
00758      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00759      MOVE PH-WD-UNLIMITED TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELGABM  
00760      MOVE 'BAMA-VALUE-QUALIFIER' TO CMF-ELEMENT-SYSTEM-NAME.      ELGABM  
00761      MOVE ACCUM-VALUE-QUALIFIER TO CMF-CODE-VALUE                 ELGABM  
00762      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
00763                                                                   ELGABM  
00764 ************************************************************      ELGABM  
00765 *                                                          *      ELGABM  
00766 *    GENERATE DOLLAR MAXIMUM TERM                          *      ELGABM  
00767 *                                                          *      ELGABM  
00768 ************************************************************      ELGABM  
00769  0290-GEN-DOLLAR-LIM-TRM.                                         ELGABM  
00770      MOVE ACCUM-VALUE-LIMIT (1) TO PH-MAX-VAL-LMT-DOLLARS.        ELGABM  
00771      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00772      MOVE PH-MAX-VAL-LMT-DOLLARS                                  ELGABM  
00773        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGABM  
00774                                                                   ELGABM  
00775 ************************************************************      ELGABM  
00776 *                                                          *      ELGABM  
00777 *    GENERATE OTHER MAXIMUM TERM                           *      ELGABM  
00778 *                                                          *      ELGABM  
00779 ************************************************************      ELGABM  
00780  0300-GEN-OTHER-LIM-TRM.                                          ELGABM  
00781      MOVE ACCUM-VALUE-LIMIT-NON-DOLLAR (1) TO PH-MAX-VAL-LMT-OTHERELGABM  
00782      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00783      MOVE PH-MAX-VAL-LMT-OTHER TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGABM  
00784                                                                   ELGABM  
00785      MOVE ACCUM-VALUE-QUALIFIER TO CMF-CODE-VALUE                 ELGABM  
00786      MOVE 'BAMA-VALUE-QUALIFIER' TO CMF-ELEMENT-SYSTEM-NAME.      ELGABM  
00787      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
00788                                                                   ELGABM  
00789 /***********************************************************      ELGABM  
00790 *                                                          *      ELGABM  
00791 *    GENERATE BENEFIT PERIOD PHRASE                        *      ELGABM  
00792 *                                                          *      ELGABM  
00793 ************************************************************      ELGABM  
00794  0310-GEN-BEN-PER-PH.                                             ELGABM  
00795 * -- CLEAR COMPRESSION INPUT BUFFER                               ELGABM  
00796      IF TCAR-FROM-SUB > 1                                         ELGABM  
00797      THEN                                                         ELGABM  
00798         PERFORM 0600-SEND-PART-PARA.                              ELGABM  
00799                                                                   ELGABM  
00800      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00801      MOVE PH-WD-PER TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGABM  
00802                                                                   ELGABM  
00803      MOVE ACCUM-BENEFIT-PERIOD   TO  CMF-CODE-VALUE.              ELGABM  
00804      MOVE 'BAMA-BENEFIT-PERIOD'  TO  CMF-ELEMENT-SYSTEM-NAME.     ELGABM  
00805      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
00806                                                                   ELGABM  
00807      IF ACCUM-BEN-PER-TIME-QUAL NOT = ZEROS                       ELGABM  
00808      THEN                                                         ELGABM  
00809         PERFORM 0320-GEN-BEN-PER-TIME-FCTR-TRM.                   ELGABM  
00810                                                                   ELGABM  
00811      IF ACCUM-INTERVAL-TIME-FCTR NOT = ZEROS                      ELGABM  
00812      THEN                                                         ELGABM  
00813         PERFORM 0330-GEN-INTRVL-TIME-FCTR-TRM.                    ELGABM  
00814                                                                   ELGABM  
00815      IF INTERVAL-OVRD-IND-NA                                      ELGABM  
00816      THEN                                                         ELGABM  
00817         CONTINUE                                                  ELGABM  
00818      ELSE                                                         ELGABM  
00819         PERFORM 0340-GEN-INTRVL-OVRD-NOTE.                        ELGABM  
00820                                                                   ELGABM  
00821 /***********************************************************      ELGABM  
00822 *                                                          *      ELGABM  
00823 *    GENERATE BENEFIT PERIOD TIME FACTOR TERM              *      ELGABM  
00824 *                                                          *      ELGABM  
00825 ************************************************************      ELGABM  
00826  0320-GEN-BEN-PER-TIME-FCTR-TRM.                                  ELGABM  
00827      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00828      MOVE PH-TIME-FCTR-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELGABM  
00829                                                                   ELGABM  
00830      MOVE ACCUM-BEN-PER-TIME-FCTR TO PH-BEN-PER-TIME-FCTR.        ELGABM  
00831      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00832      MOVE PH-BEN-PER-TIME-FCTR TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGABM  
00833                                                                   ELGABM  
00834      MOVE 'BAMA-BEN-PER-TIME-QUAL'  TO  CMF-ELEMENT-SYSTEM-NAME.  ELGABM  
00835      MOVE ACCUM-BEN-PER-TIME-QUAL   TO  CMF-CODE-VALUE.           ELGABM  
00836      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
00837                                                                   ELGABM  
00838 ************************************************************      ELGABM  
00839 *                                                          *      ELGABM  
00840 *    GENERATE INTERVAL TIME FACTOR TERM                    *      ELGABM  
00841 *                                                          *      ELGABM  
00842 ************************************************************      ELGABM  
00843  0330-GEN-INTRVL-TIME-FCTR-TRM.                                   ELGABM  
00844      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00845      MOVE PH-TIME-INTRVL-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGABM  
00846                                                                   ELGABM  
00847      MOVE ACCUM-INTERVAL-TIME-FCTR TO PH-INTRVL-TIME-FCTR.        ELGABM  
00848      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00849      MOVE PH-INTRVL-TIME-FCTR TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGABM  
00850                                                                   ELGABM  
00851      MOVE ACCUM-INTERVAL-TYPE   TO  CMF-CODE-VALUE.               ELGABM  
00852      MOVE 'BAMA-INTERVAL-TYPE'  TO  CMF-ELEMENT-SYSTEM-NAME.      ELGABM  
00853      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
00854                                                                   ELGABM  
00855 /***********************************************************      ELGABM  
00856 *                                                          *      ELGABM  
00857 *    GENERATE INTERVAL OVERRIDE NOTE                       *      ELGABM  
00858 *                                                          *      ELGABM  
00859 ************************************************************      ELGABM  
00860  0340-GEN-INTRVL-OVRD-NOTE.                                       ELGABM  
00861      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00862      MOVE PH-INTRVL-OVRD-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGABM  
00863                                                                   ELGABM  
00864      EVALUATE ACCUM-INTERVAL-OVRD-IND                             ELGABM  
00865         WHEN '1'   PERFORM 0341-GEN-INTRVL-OVRD-SPCL-PH-1         ELGABM  
00866         WHEN '2'   PERFORM 0342-GEN-INTRVL-OVRD-SPCL-PH-2         ELGABM  
00867         WHEN '3'   PERFORM 0343-GEN-INTRVL-OVRD-SPCL-PH-3         ELGABM  
00868         WHEN OTHER                                                ELGABM  
00869            MOVE ACCUM-INTERVAL-OVRD-IND TO CMF-CODE-VALUE         ELGABM  
00870            MOVE 'BAMA-INTERVAL-OVRD-IND'                          ELGABM  
00871              TO CMF-ELEMENT-SYSTEM-NAME                           ELGABM  
00872            PERFORM 0540-XLAT-ABM-CODE-VAL                         ELGABM  
00873         END-EVALUATE.                                             ELGABM  
00874                                                                   ELGABM  
00875      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00876      MOVE PH-INTRVL-OVRD-TRAIL TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGABM  
00877                                                                   ELGABM  
00878 ************************************************************      ELGABM  
00879 *                                                          *      ELGABM  
00880 *    GENERATE INTERVAL OVERRIDE SPECIAL PHRASE 1           *      ELGABM  
00881 *                                                          *      ELGABM  
00882 ************************************************************      ELGABM  
00883  0341-GEN-INTRVL-OVRD-SPCL-PH-1.                                  ELGABM  
00884      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00885      MOVE PH-INTRVL-OVRD-SPCL-1-LEAD                              ELGABM  
00886        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGABM  
00887                                                                   ELGABM  
00888      MOVE ACCUM-INTERVAL-OVRD-VALUE TO PH-INTRVL-OVRD-VAL.        ELGABM  
00889      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00890      MOVE PH-INTRVL-OVRD-VAL TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGABM  
00891                                                                   ELGABM  
00892      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00893      MOVE PH-INTRVL-OVRD-SPCL-1-TRAIL                             ELGABM  
00894        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGABM  
00895                                                                   ELGABM  
00896 ************************************************************      ELGABM  
00897 *                                                          *      ELGABM  
00898 *    GENERATE INTERVAL OVERRIDE SPECIAL PHRASE 2           *      ELGABM  
00899 *                                                          *      ELGABM  
00900 ************************************************************      ELGABM  
00901  0342-GEN-INTRVL-OVRD-SPCL-PH-2.                                  ELGABM  
00902      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00903      MOVE PH-INTRVL-OVRD-SPCL-2-LEAD                              ELGABM  
00904        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGABM  
00905                                                                   ELGABM  
00906      MOVE ACCUM-INTERVAL-OVRD-VALUE TO PH-INTRVL-OVRD-VAL.        ELGABM  
00907      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00908      MOVE PH-INTRVL-OVRD-VAL TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGABM  
00909                                                                   ELGABM  
00910      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00911      MOVE PH-INTRVL-OVRD-SPCL-2-TRAIL                             ELGABM  
00912        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGABM  
00913                                                                   ELGABM  
00914 ************************************************************      ELGABM  
00915 *                                                          *      ELGABM  
00916 *    GENERATE INTERVAL OVERRIDE SPECIAL PHRASE 3           *      ELGABM  
00917 *                                                          *      ELGABM  
00918 ************************************************************      ELGABM  
00919  0343-GEN-INTRVL-OVRD-SPCL-PH-3.                                  ELGABM  
00920      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00921      MOVE PH-INTRVL-OVRD-SPCL-3-LEAD                              ELGABM  
00922        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGABM  
00923                                                                   ELGABM  
00924      MOVE ACCUM-INTERVAL-OVRD-VALUE TO PH-INTRVL-OVRD-VAL.        ELGABM  
00925      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00926      MOVE PH-INTRVL-OVRD-VAL TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGABM  
00927                                                                   ELGABM  
00928      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00929      MOVE PH-INTRVL-OVRD-SPCL-3-TRAIL                             ELGABM  
00930        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGABM  
00931                                                                   ELGABM  
00932 /***********************************************************      ELGABM  
00933 *                                                          *      ELGABM  
00934 *    GENERATE APPLICABILITY SENTENCE 1                     *      ELGABM  
00935 *                                                          *      ELGABM  
00936 ************************************************************      ELGABM  
00937  0350-GEN-APPLIC-SENT-1.                                          ELGABM  
00938      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00939      MOVE PH-APPLIC-1-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGABM  
00940                                                                   ELGABM  
00941      MOVE ACCUM-FAM-OR-INDIV   TO  CMF-CODE-VALUE.                ELGABM  
00942      MOVE 'BAMA-FAM-OR-INDIV'  TO  CMF-ELEMENT-SYSTEM-NAME.       ELGABM  
00943      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
00944                                                                   ELGABM  
00945      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00946      MOVE PH-WD-FOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGABM  
00947                                                                   ELGABM  
00948      MOVE ACCUM-L-O-B   TO  CMF-CODE-VALUE.                       ELGABM  
00949      MOVE 'BAMA-L-O-B'  TO  CMF-ELEMENT-SYSTEM-NAME.              ELGABM  
00950      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
00951                                                                   ELGABM  
00952      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00953      MOVE PH-WD-BENEFITS TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGABM  
00954                                                                   ELGABM  
00955      IF REINSTATEMENT-IND-NA                                      ELGABM  
00956      THEN                                                         ELGABM  
00957         CONTINUE                                                  ELGABM  
00958      ELSE                                                         ELGABM  
00959         PERFORM 0360-GEN-REINST-PH.                               ELGABM  
00960                                                                   ELGABM  
00961      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00962      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGABM  
00963      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGABM  
00964                                                                   ELGABM  
00965 ************************************************************      ELGABM  
00966 *                                                          *      ELGABM  
00967 *    GENERATE REINSTATEMENT PHRASE                         *      ELGABM  
00968 *                                                          *      ELGABM  
00969 ************************************************************      ELGABM  
00970  0360-GEN-REINST-PH.                                              ELGABM  
00971      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00972      MOVE PH-REINST-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGABM  
00973                                                                   ELGABM  
00974      MOVE ACCUM-REINSTATEMENT-IND TO CMF-CODE-VALUE.              ELGABM  
00975      MOVE 'BAMA-REINSTATEMENT-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELGABM  
00976      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
00977                                                                   ELGABM  
00978 /***********************************************************      ELGABM  
00979 *                                                          *      ELGABM  
00980 *    GENERATE APPLICABILITY SENTENCE 2                     *      ELGABM  
00981 *                                                          *      ELGABM  
00982 ************************************************************      ELGABM  
00983  0370-GEN-APPLIC-SENT-2.                                          ELGABM  
00984      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00985      MOVE PH-WD-THIS TO TCAR-FROM-LINE (TCAR-FROM-SUB).           ELGABM  
00986                                                                   ELGABM  
00987      IF FYI-VALUE-NA                                              ELGABM  
00988      THEN                                                         ELGABM  
00989         CONTINUE                                                  ELGABM  
00990      ELSE                                                         ELGABM  
00991         PERFORM 0380-GEN-FYI-PH.                                  ELGABM  
00992                                                                   ELGABM  
00993      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
00994      MOVE PH-APPLIC-2-LINK TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGABM  
00995                                                                   ELGABM  
00996      IF COST-CONTAIN-IND-NA                                       ELGABM  
00997      THEN                                                         ELGABM  
00998         ADD 1 TO TCAR-FROM-SUB                                    ELGABM  
00999         MOVE PH-WD-SERVICES TO TCAR-FROM-LINE (TCAR-FROM-SUB)     ELGABM  
01000      ELSE                                                         ELGABM  
01001         PERFORM 0390-GEN-COST-CONTAINMENT-PH.                     ELGABM  
01002                                                                   ELGABM  
01003      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01004      MOVE PH-WD-FOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGABM  
01005                                                                   ELGABM  
01006      PERFORM 0400-GEN-COND-BIT-PH.                                ELGABM  
01007                                                                   ELGABM  
01008      PERFORM 0410-GEN-PLC-OF-TRTMT-PH.                            ELGABM  
01009                                                                   ELGABM  
01010      EVALUATE      AGE-LMT-FROM-IND-NA                            ELGABM  
01011               ALSO AGE-LMT-TO-IND-NA                              ELGABM  
01012               ALSO RELATIONSHIP-IND-NA                            ELGABM  
01013         WHEN TRUE ALSO TRUE ALSO TRUE                             ELGABM  
01014            CONTINUE                                               ELGABM  
01015         WHEN TRUE ALSO TRUE ALSO FALSE                            ELGABM  
01016            PERFORM 0420-GEN-PT-RLTNSHP-PH                         ELGABM  
01017         WHEN OTHER                                                ELGABM  
01018            PERFORM 0430-GEN-PT-RLTNSHP-AGE-PH                     ELGABM  
01019         END-EVALUATE.                                             ELGABM  
01020                                                                   ELGABM  
01021      IF SW-HAS-INTERNALS                                          ELGABM  
01022      THEN                                                         ELGABM  
01023         PERFORM 0460-GEN-INTRNL-TAB-LST                           ELGABM  
01024      ELSE                                                         ELGABM  
01025         CONTINUE.                                                 ELGABM  
01026                                                                   ELGABM  
01027      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01028      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGABM  
01029                                                                   ELGABM  
01030      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGABM  
01031                                                                   ELGABM  
01032 /***********************************************************      ELGABM  
01033 *                                                          *      ELGABM  
01034 *    GENERATE FYI PHRASE                                   *      ELGABM  
01035 *                                                          *      ELGABM  
01036 ************************************************************      ELGABM  
01037  0380-GEN-FYI-PH.                                                 ELGABM  
01038      MOVE ACCUM-FYI-VALUE   TO  CMF-CODE-VALUE.                   ELGABM  
01039      MOVE 'BAMA-FYI-VALUE'  TO  CMF-ELEMENT-SYSTEM-NAME.          ELGABM  
01040      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
01041                                                                   ELGABM  
01042 ************************************************************      ELGABM  
01043 *                                                          *      ELGABM  
01044 *    GENERATE COST CONTAINMENT PHRASE                      *      ELGABM  
01045 *                                                          *      ELGABM  
01046 ************************************************************      ELGABM  
01047  0390-GEN-COST-CONTAINMENT-PH.                                    ELGABM  
01048      MOVE ACCUM-COST-CONTAIN-IND   TO  CMF-CODE-VALUE.            ELGABM  
01049      MOVE 'BAMA-COST-CONTAIN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELGABM  
01050      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
01051                                                                   ELGABM  
01052 ************************************************************      ELGABM  
01053 *                                                          *      ELGABM  
01054 *    GENERATE CONDITION BITS PHRASE                        *      ELGABM  
01055 *                                                          *      ELGABM  
01056 ************************************************************      ELGABM  
01057  0400-GEN-COND-BIT-PH.                                            ELGABM  
01058                                                                   ELGABM  
01059 * -- PRESERVE CONTENTS OF TCAR-FROM-AREA                          ELGABM  
01060 *    (ELUCONDB USES THE TEXT COMPRESSION WORK AREA, WHICH CAUSES  ELGABM  
01061 *    THE LOSS OF ANYTHING IN TCAR-FROM-AREA.  WE MUST SAVE THE    ELGABM  
01062 *    CURRENT CONTENT OF TCAR-FROM-AREA AND RESTORE IT AFTER       ELGABM  
01063 *    OBTAINING THE TRANSLATION OF THE CONDITION BITS.)            ELGABM  
01064      MOVE TCAR-FROM-SUB TO WS-TEXT-HOLD-COUNT.                    ELGABM  
01065      MOVE TCAR-FROM-AREA TO WS-TEXT-HOLD-TEXT.                    ELGABM  
01066                                                                   ELGABM  
01067 * -- GET CONDITION BIT TRANSLATION                                ELGABM  
01068      MOVE ACCUM-CONDITION  TO  CMF-CONDITION-BITS.                ELGABM  
01069      CALL 'ELUCONDB' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABM  
01070                                                                   ELGABM  
01071      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGABM  
01072      CALL 'ELUSETAD'                                              ELGABM  
01073         USING DFHCOMMAREA                                         ELGABM  
01074               ADDRESS OF CMF-DESCR.                               ELGABM  
01075                                                                   ELGABM  
01076 * -- RESTORE THE CONTENTS OF TCAR-FROM-AREA                       ELGABM  
01077      MOVE WS-TEXT-HOLD-COUNT TO TCAR-FROM-SUB.                    ELGABM  
01078      MOVE WS-TEXT-HOLD-TEXT TO TCAR-FROM-AREA.                    ELGABM  
01079                                                                   ELGABM  
01080 * -- APPEND THE TRANSLATION                                       ELGABM  
01081      PERFORM 0550-MOVE-TRANSLATION-TO-COMPR.                      ELGABM  
01082                                                                   ELGABM  
01083 /***********************************************************      ELGABM  
01084 *                                                          *      ELGABM  
01085 *    GENERATE PLACE OF TREATMENT PHRASE                    *      ELGABM  
01086 *                                                          *      ELGABM  
01087 ************************************************************      ELGABM  
01088  0410-GEN-PLC-OF-TRTMT-PH.                                        ELGABM  
01089      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01090      MOVE PH-WD-PROVIDED TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGABM  
01091                                                                   ELGABM  
01092      MOVE ACCUM-PLACE-OF-TREATMENT  TO  CMF-CODE-VALUE.           ELGABM  
01093      MOVE 'BAMA-PLACE-OF-TREATMENT' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGABM  
01094      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
01095                                                                   ELGABM  
01096 ************************************************************      ELGABM  
01097 *                                                          *      ELGABM  
01098 *    GENERATE PATIENT RELATIONSHIP PHRASE                  *      ELGABM  
01099 *                                                          *      ELGABM  
01100 ************************************************************      ELGABM  
01101  0420-GEN-PT-RLTNSHP-PH.                                          ELGABM  
01102      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01103      MOVE PH-WD-TO TO TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGABM  
01104                                                                   ELGABM  
01105      MOVE ACCUM-RELATIONSHIP-IND   TO  CMF-CODE-VALUE.            ELGABM  
01106      MOVE 'BAMA-RELATIONSHIP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELGABM  
01107      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
01108                                                                   ELGABM  
01109 /***********************************************************      ELGABM  
01110 *                                                          *      ELGABM  
01111 *    GENERATE PATIENT RELATIONSHIP AND AGE PHRASE          *      ELGABM  
01112 *                                                          *      ELGABM  
01113 ************************************************************      ELGABM  
01114  0430-GEN-PT-RLTNSHP-AGE-PH.                                      ELGABM  
01115      IF RELATIONSHIP-IND-NA                                       ELGABM  
01116      THEN                                                         ELGABM  
01117         ADD 1 TO TCAR-FROM-SUB                                    ELGABM  
01118         MOVE PH-NO-PT-RLTNSHP TO TCAR-FROM-LINE (TCAR-FROM-SUB)   ELGABM  
01119      ELSE                                                         ELGABM  
01120         PERFORM 0420-GEN-PT-RLTNSHP-PH.                           ELGABM  
01121                                                                   ELGABM  
01122      PERFORM 0440-GEN-FROM-AGE-PH.                                ELGABM  
01123                                                                   ELGABM  
01124      PERFORM 0450-GEN-TO-AGE-PH.                                  ELGABM  
01125                                                                   ELGABM  
01126 /***********************************************************      ELGABM  
01127 *                                                          *      ELGABM  
01128 *    GENERATE FROM AGE PHRASE                              *      ELGABM  
01129 *                                                          *      ELGABM  
01130 ************************************************************      ELGABM  
01131  0440-GEN-FROM-AGE-PH.                                            ELGABM  
01132      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01133      MOVE PH-PT-FROM-AGE TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGABM  
01134                                                                   ELGABM  
01135      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01136      IF ACCUM-AGE-LIMIT-FROM-UNLIM                                ELGABM  
01137      THEN                                                         ELGABM  
01138         MOVE PH-WD-UNLIMITED TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELGABM  
01139      ELSE                                                         ELGABM  
01140         MOVE ACCUM-AGE-LIMIT-FROM-VAL TO PH-AGE-LIM-FROM          ELGABM  
01141         MOVE PH-AGE-LIM-FROM TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGABM  
01142                                                                   ELGABM  
01143      MOVE ACCUM-AGE-LIMIT-FROM-IND  TO  CMF-CODE-VALUE.           ELGABM  
01144      MOVE 'BAMA-AGE-QUAL-IND-FROM'  TO  CMF-ELEMENT-SYSTEM-NAME.  ELGABM  
01145      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
01146                                                                   ELGABM  
01147 ************************************************************      ELGABM  
01148 *                                                          *      ELGABM  
01149 *    GENERATE TO AGE PHRASE                                *      ELGABM  
01150 *                                                          *      ELGABM  
01151 ************************************************************      ELGABM  
01152  0450-GEN-TO-AGE-PH.                                              ELGABM  
01153      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01154      MOVE PH-PT-TO-AGE TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELGABM  
01155                                                                   ELGABM  
01156      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01157      IF ACCUM-AGE-LIMIT-TO-UNLIM                                  ELGABM  
01158      THEN                                                         ELGABM  
01159         MOVE PH-WD-UNLIMITED TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELGABM  
01160      ELSE                                                         ELGABM  
01161         MOVE ACCUM-AGE-LIMIT-TO-VAL TO PH-AGE-LIM-TO              ELGABM  
01162         MOVE PH-AGE-LIM-TO TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGABM  
01163                                                                   ELGABM  
01164      MOVE ACCUM-AGE-LIMIT-TO-IND    TO  CMF-CODE-VALUE.           ELGABM  
01165      MOVE 'BAMA-AGE-QUAL-IND-TO'    TO  CMF-ELEMENT-SYSTEM-NAME.  ELGABM  
01166      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
01167                                                                   ELGABM  
01168 /***********************************************************      ELGABM  
01169 *                                                          *      ELGABM  
01170 *    GENERATE INTERNAL TABULARS LIST                       *      ELGABM  
01171 *                                                          *      ELGABM  
01172 ************************************************************      ELGABM  
01173  0460-GEN-INTRNL-TAB-LST.                                         ELGABM  
01174      INITIALIZE WS-INT-TAB-LST                                    ELGABM  
01175                 WS-INT-TAB-TXT                                    ELGABM  
01176                 WS-TAB-SUB.                                       ELGABM  
01177                                                                   ELGABM  
01178      IF SW-HAS-IBGR                                               ELGABM  
01179         ADD 1 TO WS-TAB-SUB                                       ELGABM  
01180         SET WS-INT-TAB-IBGR (WS-TAB-SUB)                          ELGABM  
01181             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGABM  
01182                                                                   ELGABM  
01183      IF SW-HAS-IDGD                                               ELGABM  
01184         ADD 1 TO WS-TAB-SUB                                       ELGABM  
01185         SET WS-INT-TAB-IDGD (WS-TAB-SUB)                          ELGABM  
01186             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGABM  
01187                                                                   ELGABM  
01188      IF SW-HAS-IPGN                                               ELGABM  
01189         ADD 1 TO WS-TAB-SUB                                       ELGABM  
01190         SET WS-INT-TAB-IPGN (WS-TAB-SUB)                          ELGABM  
01191             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGABM  
01192                                                                   ELGABM  
01193      IF SW-HAS-IPGP                                               ELGABM  
01194         ADD 1 TO WS-TAB-SUB                                       ELGABM  
01195         SET WS-INT-TAB-IPGP (WS-TAB-SUB)                          ELGABM  
01196             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGABM  
01197                                                                   ELGABM  
01198      IF SW-HAS-IPGT                                               ELGABM  
01199         ADD 1 TO WS-TAB-SUB                                       ELGABM  
01200         SET WS-INT-TAB-IPGT (WS-TAB-SUB)                          ELGABM  
01201             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGABM  
01202                                                                   ELGABM  
01203      IF SW-HAS-IPGS                                               ELGABM  
01204         ADD 1 TO WS-TAB-SUB                                       ELGABM  
01205         SET WS-INT-TAB-IPGS (WS-TAB-SUB)                          ELGABM  
01206             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGABM  
01207                                                                   ELGABM  
01208      IF WS-TAB-SUB > 0                                            ELGABM  
01209         SET WS-INT-TAB-END (WS-TAB-SUB) TO TRUE                   ELGABM  
01210         IF  WS-TAB-SUB > 1                                        ELGABM  
01211         THEN                                                      ELGABM  
01212            SET WS-INT-TAB-AND (WS-TAB-SUB - 1) TO TRUE            ELGABM  
01213         END-IF                                                    ELGABM  
01214         ADD 1 TO TCAR-FROM-SUB                                    ELGABM  
01215         MOVE PH-INTRNL-TAB-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB) ELGABM  
01216         ADD 1 TO TCAR-FROM-SUB                                    ELGABM  
01217         MOVE WS-INT-TAB-TXT-1 TO TCAR-FROM-LINE (TCAR-FROM-SUB)   ELGABM  
01218         IF WS-TAB-SUB > 3                                         ELGABM  
01219         THEN                                                      ELGABM  
01220            ADD 1 TO TCAR-FROM-SUB                                 ELGABM  
01221            MOVE WS-INT-TAB-TXT-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB)ELGABM  
01222         END-IF                                                    ELGABM  
01223         ADD 1 TO TCAR-FROM-SUB                                    ELGABM  
01224         MOVE PH-INTRNL-TAB-TRAIL TO TCAR-FROM-LINE (TCAR-FROM-SUB)ELGABM  
01225      END-IF.                                                      ELGABM  
01226                                                                   ELGABM  
01227 /***********************************************************      ELGABM  
01228 *                                                          *      ELGABM  
01229 *    GENERATE DEFINITION SENTENCE                          *      ELGABM  
01230 *                                                          *      ELGABM  
01231 ************************************************************      ELGABM  
01232  0510-GEN-DEFN-SENT.                                              ELGABM  
01233      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01234      MOVE PH-DEFN-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELGABM  
01235                                                                   ELGABM  
01236      MOVE ACCUM-DEFINITION      TO CMF-CODE-VALUE.                ELGABM  
01237      MOVE 'BAMA-DEFINITION'     TO CMF-ELEMENT-SYSTEM-NAME.       ELGABM  
01238      PERFORM 0540-XLAT-ABM-CODE-VAL.                              ELGABM  
01239                                                                   ELGABM  
01240      ADD 1 TO TCAR-FROM-SUB.                                      ELGABM  
01241      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGABM  
01242                                                                   ELGABM  
01243      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGABM  
01244                                                                   ELGABM  
01245 /***********************************************************      ELGABM  
01246 *                                                          *      ELGABM  
01247 *    GENERATE TOPIC MAXIMUM OCCURRENCE TRAILER             *      ELGABM  
01248 *                                                          *      ELGABM  
01249 ************************************************************      ELGABM  
01250  0520-GEN-TOPIC-TRAILER.                                          ELGABM  
01251      IF TCAR-FROM-SUB > 0                                         ELGABM  
01252      THEN                                                         ELGABM  
01253         PERFORM 0610-COMPLETE-AND-SEND-PARA.                      ELGABM  
01254      ADD 1 TO TCAR-FROM-SUB                                       ELGABM  
01255      MOVE PH-SEE-BP-TOPICS-1 TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGABM  
01256      ADD 1 TO TCAR-FROM-SUB                                       ELGABM  
01257      MOVE PH-SEE-BP-TOPICS-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGABM  
01258      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGABM  
01259                                                                   ELGABM  
01260 ************************************************************      ELGABM  
01261 *                                                          *      ELGABM  
01262 *    GENERATE BENEFIT PROVISION MAXIMUM OCCURRENCE TRAILER *      ELGABM  
01263 *                                                          *      ELGABM  
01264 ************************************************************      ELGABM  
01265  0530-GEN-BP-TRAILER.                                             ELGABM  
01266      IF TCAR-FROM-SUB > 0                                         ELGABM  
01267      THEN                                                         ELGABM  
01268         PERFORM 0610-COMPLETE-AND-SEND-PARA.                      ELGABM  
01269      ADD 1 TO TCAR-FROM-SUB                                       ELGABM  
01270      MOVE PH-SEE-MAX-TOPIC TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGABM  
01271      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGABM  
01272                                                                   ELGABM  
01273 /***********************************************************      ELGABM  
01274 *                                                          *      ELGABM  
01275 *    TRANSLATE ACCUMULATOR CODE VALUE                      *      ELGABM  
01276 *    AND MOVE TO COMPRESS WORK AREA                        *      ELGABM  
01277 *                                                          *      ELGABM  
01278 ************************************************************      ELGABM  
01279  0540-XLAT-ABM-CODE-VAL.                                          ELGABM  
01280      MOVE '#ABM' TO CMF-RECORD-PREFIX.                            ELGABM  
01281      EXEC CICS LINK PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)      ELGABM  
01282         END-EXEC.                                                 ELGABM  
01283                                                                   ELGABM  
01284      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGABM  
01285      CALL 'ELUSETAD'                                              ELGABM  
01286         USING DFHCOMMAREA                                         ELGABM  
01287               ADDRESS OF CMF-DESCR.                               ELGABM  
01288                                                                   ELGABM  
01289      PERFORM 0550-MOVE-TRANSLATION-TO-COMPR.                      ELGABM  
01290                                                                   ELGABM  
01291 ************************************************************      ELGABM  
01292 *                                                          *      ELGABM  
01293 *        MOVE TRANSLATION TO COMPRESS AREA                 *      ELGABM  
01294 *                                                          *      ELGABM  
01295 ************************************************************      ELGABM  
01296  0550-MOVE-TRANSLATION-TO-COMPR.                                  ELGABM  
01297      PERFORM WITH TEST BEFORE                                     ELGABM  
01298            VARYING CMF-DESCR-IDX FROM 1 BY 1                      ELGABM  
01299              UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES            ELGABM  
01300         IF TCAR-FROM-SUB >= 20                                    ELGABM  
01301         THEN                                                      ELGABM  
01302            PERFORM 0600-SEND-PART-PARA                            ELGABM  
01303         END-IF                                                    ELGABM  
01304         ADD 1 TO TCAR-FROM-SUB                                    ELGABM  
01305         MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                       ELGABM  
01306           TO TCAR-FROM-LINE (TCAR-FROM-SUB)                       ELGABM  
01307         END-PERFORM.                                              ELGABM  
01308                                                                   ELGABM  
01309 /***********************************************************      ELGABM  
01310 *                                                          *      ELGABM  
01311 *    SEND A PARTIAL PARAGRAPH TO OUTPUT                    *      ELGABM  
01312 *                                                          *      ELGABM  
01313 ************************************************************      ELGABM  
01314  0600-SEND-PART-PARA.                                             ELGABM  
01315 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGABM  
01316      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGABM  
01317 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGABM  
01318      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGABM  
01319      MOVE +79                                                     ELGABM  
01320        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGABM  
01321           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGABM  
01322           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGABM  
01323           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGABM  
01324           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGABM  
01325           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGABM  
01326           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGABM  
01327           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGABM  
01328           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGABM  
01329           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGABM  
01330 * -- UNSTRING/FLOW THE OUTPUT                                     ELGABM  
01331      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGABM  
01332 * -- MOVE FORMATTED TEXT TO OUTPUT ** EXCEPT LAST LINE **         ELGABM  
01333      PERFORM WITH TEST BEFORE                                     ELGABM  
01334            VARYING TCAR-X FROM 1 BY 1                             ELGABM  
01335              UNTIL TCAR-X = TCAR-OUTPUT-FIELDS-USED               ELGABM  
01336 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGABM  
01337         IF COF-NBR-DTL-LINES >= 20                                ELGABM  
01338         THEN                                                      ELGABM  
01339            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGABM  
01340            INITIALIZE COF-DTL                                     ELGABM  
01341            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGABM  
01342         END-IF                                                    ELGABM  
01343 *    -- APPEND LINE TO OUTPUT                                     ELGABM  
01344         ADD 1 TO COF-NBR-DTL-LINES                                ELGABM  
01345         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGABM  
01346           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGABM  
01347         END-PERFORM.                                              ELGABM  
01348 * -- PUT LAST LINE OF COMPRESSED/UNSTRUNG OUTPUT INTO FROM AREA   ELGABM  
01349      INITIALIZE TCAR-FROM-AREA                                    ELGABM  
01350                 TCAR-FROM-LENGTH                                  ELGABM  
01351                 TCAR-FROM-SUB.                                    ELGABM  
01352      MOVE 1 TO TCAR-FROM-SUB.                                     ELGABM  
01353      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELGABM  
01354        TO TCAR-FROM-LINE (1).                                     ELGABM  
01355                                                                   ELGABM  
01356 /***********************************************************      ELGABM  
01357 *                                                          *      ELGABM  
01358 *    COMPLETE AND SEND A PARAGRAPH TO OUTPUT               *      ELGABM  
01359 *                                                          *      ELGABM  
01360 ************************************************************      ELGABM  
01361  0610-COMPLETE-AND-SEND-PARA.                                     ELGABM  
01362 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGABM  
01363      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGABM  
01364 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGABM  
01365      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGABM  
01366      MOVE +79                                                     ELGABM  
01367        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGABM  
01368           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGABM  
01369           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGABM  
01370           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGABM  
01371           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGABM  
01372           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGABM  
01373           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGABM  
01374           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGABM  
01375           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGABM  
01376           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGABM  
01377 * -- UNSTRING/FLOW THE OUTPUT                                     ELGABM  
01378      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGABM  
01379 * -- MOVE FORMATTED TEXT TO OUTPUT                                ELGABM  
01380      PERFORM WITH TEST AFTER                                      ELGABM  
01381            VARYING TCAR-X FROM 1 BY 1                             ELGABM  
01382              UNTIL TCAR-X > TCAR-OUTPUT-FIELDS-USED               ELGABM  
01383 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGABM  
01384         IF COF-NBR-DTL-LINES >= 20                                ELGABM  
01385         THEN                                                      ELGABM  
01386            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGABM  
01387            INITIALIZE COF-DTL                                     ELGABM  
01388            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGABM  
01389         END-IF                                                    ELGABM  
01390 *    -- APPEND LINE TO OUTPUT                                     ELGABM  
01391         ADD 1 TO COF-NBR-DTL-LINES                                ELGABM  
01392         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGABM  
01393           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGABM  
01394         END-PERFORM.                                              ELGABM  
01395 * -- INSERT A BLANK LINE                                          ELGABM  
01396      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGABM  
01397      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELGABM  
01398 * -- CALL THE OUTPUT MODULE                                       ELGABM  
01399      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA.                  ELGABM  
01400      INITIALIZE COF-DTL.                                          ELGABM  
01401      MOVE ZERO TO COF-NBR-DTL-LINES.                              ELGABM  
01402                                                                   ELGABM  
01403 * -- CLEAR THE COMPRESSION WORK AREA                              ELGABM  
01404      INITIALIZE TCAR-FROM-AREA                                    ELGABM  
01405                 TCAR-FROM-SUB.                                    ELGABM  
01406                                                                   ELGABM  
01407 /***********************************************************      ELGABM  
01408 *                                                          *      ELGABM  
01409 *    GENERATE INTERNAL TABULAR LISTINGS                    *      ELGABM  
01410 *                                                          *      ELGABM  
01411 ************************************************************      ELGABM  
01412  0650-GEN-INTRNL-TAB-LSTNGS.                                      ELGABM  
01413                                                                   ELGABM  
01414      IF SW-HAS-IBGR                                               ELGABM  
01415         MOVE PC-IBGR TO SRP-INTERNAL-TAB-ID                       ELGABM  
01416         EXEC CICS LINK PROGRAM ('ELGIBGR')                        ELGABM  
01417                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGABM  
01418                                                                   ELGABM  
01419      IF SW-HAS-IDGD                                               ELGABM  
01420         MOVE PC-IDGD TO SRP-INTERNAL-TAB-ID                       ELGABM  
01421         EXEC CICS LINK PROGRAM ('ELGIDGD')                        ELGABM  
01422                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGABM  
01423                                                                   ELGABM  
01424      IF SW-HAS-IPGN                                               ELGABM  
01425         MOVE PC-IPGN TO SRP-INTERNAL-TAB-ID                       ELGABM  
01426         EXEC CICS LINK PROGRAM ('ELGIPGN')                        ELGABM  
01427                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGABM  
01428                                                                   ELGABM  
01429      IF SW-HAS-IPGP                                               ELGABM  
01430         MOVE PC-IPGP TO SRP-INTERNAL-TAB-ID                       ELGABM  
01431         EXEC CICS LINK PROGRAM ('ELGIPGP')                        ELGABM  
01432                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGABM  
01433                                                                   ELGABM  
01434      IF SW-HAS-IPGT                                               ELGABM  
01435         MOVE PC-IPGT TO SRP-INTERNAL-TAB-ID                       ELGABM  
01436         EXEC CICS LINK PROGRAM ('ELGIPGT')                        ELGABM  
01437                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGABM  
01438                                                                   ELGABM  
01439      IF SW-HAS-IPGS                                               ELGABM  
01440         MOVE PC-IPGS TO SRP-INTERNAL-TAB-ID                       ELGABM  
01441         EXEC CICS LINK PROGRAM ('ELGIPGS')                        ELGABM  
01442                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGABM  
01443                                                                   ELGABM  
01444 /***********************************************************      ELGABM  
01445 *                                                          *      ELGABM  
01446 *    READ ACCUMULATOR WORK FILE RECORD                     *      ELGABM  
01447 *                                                          *      ELGABM  
01448 ************************************************************      ELGABM  
01449                                                                   ELGABM  
01450  0660-READ-ACCUM-WORK-FILE-REC.                                   ELGABM  
01451      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGABM  
01452      CALL 'ELUSETAD'                                              ELGABM  
01453          USING DFHCOMMAREA                                        ELGABM  
01454                ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.            ELGABM  
01455      SET IOP-FCQ-NONE         TO  TRUE.                           ELGABM  
01456      SET IOP-KVQ-NONE         TO  TRUE.                           ELGABM  
01457      SET IOP-STG-MODE-LOCATE  TO  TRUE.                           ELGABM  
01458      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABM  
01459      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR.       ELGABM  
01460                                                                   ELGABM  
01461 ************************************************************      ELGABM  
01462 *                                                          *      ELGABM  
01463 *    DELETE ACCUMULATOR WORK FILE                          *      ELGABM  
01464 *                                                          *      ELGABM  
01465 ************************************************************      ELGABM  
01466                                                                   ELGABM  
01467  0670-DELETE-ACCUM-WORK-FILE.                                     ELGABM  
01468      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGABM  
01469      CALL 'ELUSETAD'                                              ELGABM  
01470         USING DFHCOMMAREA                                         ELGABM  
01471               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELGABM  
01472      SET  IOP-DEL           TO  TRUE.                             ELGABM  
01473      SET  IOP-FCQ-NONE      TO  TRUE.                             ELGABM  
01474      SET  IOP-KVQ-NONE      TO  TRUE.                             ELGABM  
01475      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABM  
01476                                                                   ELGABM  
01477 /***********************************************************      ELGABM  
01478 *                                                          *      ELGABM  
01479 *        END THE DISPLAY                                   *      ELGABM  
01480 *                                                          *      ELGABM  
01481 ************************************************************      ELGABM  
01482  0680-END-THE-DISPLAY.                                            ELGABM  
01483      IF COF-NBR-DTL-LINES > 0                                     ELGABM  
01484         CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.      ELGABM  
01485      SET COF-END  TO  TRUE.                                       ELGABM  
01486      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELGABM  
01487      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGABM  
01488      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGABM  
