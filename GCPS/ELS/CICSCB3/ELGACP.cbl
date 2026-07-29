00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGACP  
00003  PROGRAM-ID.           ELGACP.                                       LV002
00004                                                                   ELGACP  
00005  AUTHOR.               ANNE KEFFER KING.                          ELGACP  
00006                                                                   ELGACP  
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELGACP  
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELGACP  
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELGACP  
00010                        233 N. MICHIGAN AVE                        ELGACP  
00011                        CHICAGO, ILLINOIS 60601                    ELGACP  
00012                                                                   ELGACP  
00013  DATE-WRITTEN.         10-JUN-1992.                               ELGACP  
00014                                                                   ELGACP  
00015  DATE-COMPILED.                                                   ELGACP  
00016                                                                   ELGACP  
00017  SECURITY.             COPYRIGHT 1986, 1991                       ELGACP  
00018                        HEALTH CARE SERVICE CORPORATION            ELGACP  
00019      SKIP3                                                        ELGACP  
00020  ENVIRONMENT DIVISION.                                            ELGACP  
00021                                                                   ELGACP  
00022  CONFIGURATION SECTION.                                           ELGACP  
00023  SOURCE-COMPUTER.      IBM-3090.                                  ELGACP  
00024  OBJECT-COMPUTER.      IBM-3090.                                  ELGACP  
00025 /*****************************************************************ELGACP  
00026 *                                                                *ELGACP  
00027 *  ELGACP   - ELS:  GENERATES THE OUTPUT FOR COPPAYS AT          *ELGACP  
00028 *                   COST CONTAINMENT, TOPIC, AND THE BENEFIT     *ELGACP  
00029 *                   PROVISION LEVEL.                             *ELGACP  
00030 *                                                                *ELGACP  
00031 ******************************************************************ELGACP  
00032 *                                                                *ELGACP  
00033 *                      MAINTENANCE HISTORY                       *ELGACP  
00034 *                                                                *ELGACP  
00035 *  MOD     DATE     BY  DRPT                ACTION               *ELGACP  
00036 * ----- ----------- --- ----- ---------------------------------- *ELGACP  
00037 * 01.00 21-OCT-1998 AKK       CLONED FROM ELGADL.                *ELGACP  
00038 *                                                                *ELGACP  
00039 * 01.02 24-AUG-2000 AKK       ADD SUPPORT FOR ACP                *ELGACP  
00040 *                                                                *ELGACP  
00041 * 01.03 01-JAN-2003 AKK       COMPILE FOR ADDITON OF SMI NSM     *ELGACP  
00042 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGACP  
ED0624*                                                                *        
ED0624* BBDA-58217 06/14/24  ED     RECOMPILE FOR PEAQ COPYBOOK        *        
ED0624*                             EXPANSION:                         *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00043 ******************************************************************ELGACP  
00044      TITLE 'WORKING STORAGE SECTION'.                             ELGACP  
00045  DATA DIVISION.                                                   ELGACP  
00046  WORKING-STORAGE SECTION.                                         ELGACP  
00047                                                                   ELGACP  
00048 * -- HEADERS                                                      ELGACP  
00049                                                                   ELGACP  
00050   01  HD-TOPIC-HDR .                                              ELGACP  
00051       02                          PICTURE  X(36) VALUE SPACES.    ELGACP  
00052       02                          PICTURE  X(07) VALUE            ELGACP  
00053          'COPAYS'.                                                ELGACP  
00054       02                          PICTURE  X(36) VALUE SPACES.    ELGACP  
00055                                                                   ELGACP  
00056   01  HD-BEN-PROVN-HDR.                                           ELGACP  
00057       02                          PICTURE  X(24) VALUE            ELGACP  
00058          'COPAY AT BENEFIT LEVEL:'.                               ELGACP  
00059       02                          PICTURE  X(55) VALUE SPACES.    ELGACP  
00060                                                                   ELGACP  
00061 * -- PHRASES AND TERMS - CONSTANT                                 ELGACP  
00062                                                                   ELGACP  
00063  01  WS-COLON                    PICTURE  X(1) VALUE ':'.         ELGACP  
00064  01  WS-PERIOD                   PICTURE  X(1) VALUE '.'.         ELGACP  
00065                                                                   ELGACP  
00066  01  WS-ACCUMULATING-PHRASE      PICTURE  X(38) VALUE             ELGACP  
00067      ', ACCUMULATING TO AN OVERALL COPAY OF '.                    ELGACP  
00068                                                                   ELGACP  
00069  01  PH-APPLIC-1-LEAD            PICTURE  X(14) VALUE             ELGACP  
00070      'THIS COPAY OF '.                                            ELGACP  
00071                                                                   ELGACP  
00072  01  PH-APPLIC-1-LEAD-C          PICTURE  X(23) VALUE             ELGACP  
00073      'THIS COPAY APPLIES PER '.                                   ELGACP  
00074                                                                   ELGACP  
00075  01  PH-APPLIES                  PICTURE  X(08) VALUE             ELGACP  
00076      'APPLIES '.                                                  ELGACP  
00077                                                                   ELGACP  
00078  01  PH-APPLIC-2-LINK            PICTURE  X(17) VALUE             ELGACP  
00079      'COPAY APPLIES TO'.                                          ELGACP  
00080                                                                   ELGACP  
00081  01  PH-CARRY-OVER               PICTURE  X(33) VALUE             ELGACP  
00082      'THE COPAY CARRY OVER CREDIT IS: '.                          ELGACP  
00083                                                                   ELGACP  
00084  01  PH-DEFN-LEAD                PICTURE  X(32) VALUE             ELGACP  
00085      'DOLLARS ARE ACCUMULATED BASED ON'.                          ELGACP  
00086                                                                   ELGACP  
00087  01  PH-INTRNL-TAB-LEAD          PICTURE  X(12) VALUE             ELGACP  
00088      ', SUBJECT TO'.                                              ELGACP  
00089                                                                   ELGACP  
00090  01  PH-INTRNL-TAB-TRAIL         PICTURE  X(27) VALUE             ELGACP  
00091      'CONSIDERATIONS LISTED BELOW'.                               ELGACP  
00092                                                                   ELGACP  
00093  01  PH-INTRVL-OVRD-LEAD         PICTURE  X(06) VALUE             ELGACP  
00094      '(NOTE:'.                                                    ELGACP  
00095                                                                   ELGACP  
00096  01  PH-INTRVL-OVRD-TRAIL        PICTURE  X(02) VALUE             ELGACP  
00097      ').'.                                                        ELGACP  
00098                                                                   ELGACP  
00099  01  PH-INTRVL-OVRD-SPCL-1-LEAD  PICTURE  X(32) VALUE             ELGACP  
00100      'THE INTERVAL MAY BE OVERRULED IF'.                          ELGACP  
00101                                                                   ELGACP  
00102  01  PH-INTRVL-OVRD-SPCL-1-TRAIL PICTURE  X(75) VALUE             ELGACP  
00103      'MONTHS HAVE ELAPSED FROM THE ADMISSION DATE OF THE FIRST COVELGACP  
00104 -    'ERED ADMISSION.'.                                           ELGACP  
00105                                                                   ELGACP  
00106  01  PH-INTRVL-OVRD-SPCL-2-LEAD  PICTURE  X(72) VALUE             ELGACP  
00107      'IF THE MEMBER IS MEDICARE ELIGIBLE, THE BENEFIT PERIODS ARE ELGACP  
00108 -    'SEPARATED BY'.                                              ELGACP  
00109                                                                   ELGACP  
00110  01  PH-INTRVL-OVRD-SPCL-2-TRAIL PICTURE  X(05) VALUE             ELGACP  
00111      'DAYS.'.                                                     ELGACP  
00112                                                                   ELGACP  
00113  01  PH-INTRVL-OVRD-SPCL-3-LEAD  PICTURE  X(37) VALUE             ELGACP  
00114      'THE INTERVAL CAN BE OVERRULED SO THAT'.                     ELGACP  
00115                                                                   ELGACP  
00116  01  PH-INTRVL-OVRD-SPCL-3-TRAIL PICTURE  X(52) VALUE             ELGACP  
00117      'DAYS/VISITS ARE PAID AT THE INDICATED PERCENT LEVEL.'.      ELGACP  
00118                                                                   ELGACP  
00119  01  PH-ACP-VAL-SENT-LEAD         PICTURE  X(22) VALUE            ELGACP  
00120      'THERE IS A COPAY '.                                         ELGACP  
00121                                                                   ELGACP  
00122  01  PH-ACP-OTHR-SRCE-LEAD        PICTURE  X(09) VALUE            ELGACP  
00123      'FOUND IN '.                                                 ELGACP  
00124                                                                   ELGACP  
00125  01  PH-ACP-OTHR-SRCE-UNKN        PICTURE  X(15) VALUE            ELGACP  
00126      'ANOTHER SOURCE'.                                            ELGACP  
00127                                                                   ELGACP  
00128  01  WS-AS-FOLLOWS                PICTURE  X(10) VALUE            ELGACP  
00129      'AS FOLLOWS'.                                                ELGACP  
00130                                                                   ELGACP  
00131  01  WS-PER-CLAIM                 PICTURE  X(10) VALUE            ELGACP  
00132      'PER CLAIM '.                                                ELGACP  
00133                                                                   ELGACP  
00134  01  WS-PER                      PICTURE  X(04) VALUE             ELGACP  
00135      'PER '.                                                      ELGACP  
00136                                                                   ELGACP  
00137  01  WS-PER-DATE                  PICTURE  X(09) VALUE            ELGACP  
00138      'PER DATE '.                                                 ELGACP  
00139                                                                   ELGACP  
00140  01  WS-PER-CALENDER-YEAR         PICTURE  X(18) VALUE            ELGACP  
00141      'PER CALENDER YEAR '.                                        ELGACP  
00142                                                                   ELGACP  
00143  01  WS-PER-CONTRACT-YEAR         PICTURE  X(18) VALUE            ELGACP  
00144      'PER CONTRACT YEAR '.                                        ELGACP  
00145                                                                   ELGACP  
00146  01  WS-PER-LIFETIME              PICTURE  X(13) VALUE            ELGACP  
00147      'PER LIFETIME '.                                             ELGACP  
00148                                                                   ELGACP  
00149  01  PH-NO-ACP.                                                   ELGACP  
00150      02 FILLER                   PICTURE  X(42) VALUE             ELGACP  
00151      'NO GROUP OR CONTRACT LEVEL COPAYS APPLY,'.                  ELGACP  
00152      02 FILLER                   PICTURE  X(35) VALUE             ELGACP  
00153      ' PLEASE CHECK THE DEDUCTIBLE TOPIC.'.                       ELGACP  
00154                                                                   ELGACP  
00155  01  PH-NO-INST-ACP.                                              ELGACP  
00156      02 FILLER                   PICTURE  X(35) VALUE             ELGACP  
00157           'NO INSTITUTIONAL GROUP OR CONTRACT '.                  ELGACP  
00158      02 FILLER                   PICTURE  X(21) VALUE             ELGACP  
00159           'LEVEL COPAYS APPLY.'.                                  ELGACP  
00160                                                                   ELGACP  
00161  01  PH-NO-PROF-ACP.                                              ELGACP  
00162      02 FILLER                   PICTURE  X(34) VALUE             ELGACP  
00163           'NO PROFESSIONAL GROUP OR CONTRACT '.                   ELGACP  
00164      02 FILLER                   PICTURE  X(19) VALUE             ELGACP  
00165           'LEVEL COPAY APPLY.'.                                   ELGACP  
00166                                                                   ELGACP  
00167  01  PH-NO-PT-RLTNSHP            PICTURE  X(11) VALUE             ELGACP  
00168      'TO PATIENTS'.                                               ELGACP  
00169                                                                   ELGACP  
00170  01  PH-PT-FROM-AGE              PICTURE  X(08) VALUE             ELGACP  
00171      'FROM AGE'.                                                  ELGACP  
00172                                                                   ELGACP  
00173  01  PH-PT-TO-AGE                PICTURE  X(06) VALUE             ELGACP  
00174      'TO AGE'.                                                    ELGACP  
00175                                                                   ELGACP  
00176  01  PH-REINST-LEAD              PICTURE  X(08) VALUE             ELGACP  
00177      ', AND IS'.                                                  ELGACP  
00178                                                                   ELGACP  
00179  01  PH-TIME-FCTR-LEAD           PICTURE  X(09) VALUE             ELGACP  
00180      'PERIOD OF'.                                                 ELGACP  
00181                                                                   ELGACP  
00182  01  PH-TIME-INTRVL-LEAD         PICTURE  X(12) VALUE             ELGACP  
00183      'SEPARATED BY'.                                              ELGACP  
00184                                                                   ELGACP  
00185  01  PH-SEE-BP-TOPICS-1.                                          ELGACP  
00186      02 FILLER                   PICTURE  X(27) VALUE             ELGACP  
00187      'ADDITIONAL COPAYS MAY '.                                    ELGACP  
00188      02 FILLER                   PICTURE  X(29) VALUE             ELGACP  
00189      'APPLY TO INDIVIDUAL BENEFITS.'.                             ELGACP  
00190                                                                   ELGACP  
00191  01  PH-SEE-BP-TOPICS-2          PICTURE  X(47) VALUE             ELGACP  
00192      'SEE SPECIFIC TOPICS FOR ADDITIONAL COPAYS.'.                ELGACP  
00193                                                                   ELGACP  
00194  01  PH-SEE-ACP-TOPIC             PICTURE  X(53) VALUE            ELGACP  
00195      'SEE THE COPAYS TOPIC FOR ADDITIONAL COPAYS.'.               ELGACP  
00196                                                                   ELGACP  
00197  01  PH-CHANGING-TO               PICTURE  X(17) VALUE            ELGACP  
00198      'THEN CHANGING TO '.                                         ELGACP  
00199                                                                   ELGACP  
00200  01  PH-UP-TO                     PICTURE  X(8) VALUE             ELGACP  
00201      ' UP TO '.                                                   ELGACP  
00202                                                                   ELGACP  
00203 * -- PHRASES AND TERMS - SINGLE WORDS                             ELGACP  
00204                                                                   ELGACP  
00205  01  PH-WD-BENEFITS    PICTURE  X(10) VALUE 'BENEFITS '.          ELGACP  
00206  01  PH-WD-FOR         PICTURE  X(04) VALUE 'FOR '.               ELGACP  
00207  01  PH-WD-OF          PICTURE  X(03) VALUE 'OF '.                ELGACP  
00208  01  PH-WD-PER         PICTURE  X(04) VALUE 'PER '.               ELGACP  
00209  01  PH-WD-PATIENTS    PICTURE  X(09) VALUE 'PATIENTS '.          ELGACP  
00210  01  PH-WD-PROVIDED    PICTURE  X(09) VALUE 'PROVIDED '.          ELGACP  
00211  01  PH-WD-SERVICES    PICTURE  X(09) VALUE 'SERVICES '.          ELGACP  
00212  01  PH-WD-THE         PICTURE  X(04) VALUE 'THE '.               ELGACP  
00213  01  PH-WD-THIS        PICTURE  X(05) VALUE 'THIS '.              ELGACP  
00214  01  PH-WD-TO          PICTURE  X(03) VALUE 'TO '.                ELGACP  
00215  01  PH-WD-AT          PICTURE  X(15) VALUE '             AT'.    ELGACP  
00216  01  PH-WD-UNLIMITED   PICTURE  X(10) VALUE 'UNLIMITED '.         ELGACP  
00217                                                                   ELGACP  
00218 * -- PHRASES AND TERMS - NUMERIC FORMAT AREAS                     ELGACP  
00219                                                                   ELGACP  
00220  01  PH-AGE-LIM-FROM             PICTURE  ZZ9B.                   ELGACP  
00221                                                                   ELGACP  
00222  01  PH-AGE-LIM-TO               PICTURE  ZZ9B.                   ELGACP  
00223                                                                   ELGACP  
00224  01  PH-BEN-PER-TIME-FCTR        PICTURE  ZZ9B.                   ELGACP  
00225                                                                   ELGACP  
00226  01  PH-INTRVL-TIME-FCTR         PICTURE  ZZ9B.                   ELGACP  
00227                                                                   ELGACP  
00228  01  PH-INTRVL-OVRD-VAL          PICTURE  ZZZZ9B.                 ELGACP  
00229                                                                   ELGACP  
00230  01  PH-ACP-VAL-LMT-DOLLARS      PICTURE  $$,$$$,$$9.99B.         ELGACP  
00231                                                                   ELGACP  
00232  01  PH-ACP-VAL-LMT-OTHER        PICTURE  ZZZ,ZZZ,Z99B.           ELGACP  
00233                                                                   ELGACP  
00234 * -- OTHERS                                                       ELGACP  
00235                                                                   ELGACP  
00236  01  PROGRAM-CONSTANTS.                                           ELGACP  
00237      05  PC-ADL                      PIC  X(06) VALUE             ELGACP  
00238              '#ADL  '.                                            ELGACP  
00239      05  PC-IBGR                     PIC  X(06) VALUE             ELGACP  
00240              '#IBGR '.                                            ELGACP  
00241      05  PC-IDGD                     PIC  X(06) VALUE             ELGACP  
00242              '#IDGD '.                                            ELGACP  
00243      05  PC-IPGN                     PIC  X(06) VALUE             ELGACP  
00244              '#IPGN '.                                            ELGACP  
00245      05  PC-IPGP                     PIC  X(06) VALUE             ELGACP  
00246              '#IPGP '.                                            ELGACP  
00247      05  PC-IPGT                     PIC  X(06) VALUE             ELGACP  
00248              '#IPGT '.                                            ELGACP  
00249      05  PC-IPGS                     PIC  X(06) VALUE             ELGACP  
00250              '#IPGS '.                                            ELGACP  
00251       05 PC-CHANGES-TO               PIC  X(16) VALUE             ELGACP  
00252               'THEN CHANGES TO '.                                 ELGACP  
00253                                                                   ELGACP  
00254 * -- INTERNAL TABULAR INFORMATION WORK AREAS                      ELGACP  
00255 * -- INTERNAL TABULAR SWITCHES                                    ELGACP  
00256                                                                   ELGACP  
00257  01  WS-INDEXES.                                                  ELGACP  
00258      05  WS-INDEX                  USAGE IS INDEX.                ELGACP  
00259      05  AA-INDEX                  USAGE IS INDEX.                ELGACP  
00260                                                                   ELGACP  
00261  01  WS-INT-TAB-SW.                                               ELGACP  
00262      02                          PICTURE  X(01).                  ELGACP  
00263         88 SW-HAS-INTERNALS      VALUE 'Y'.                       ELGACP  
00264         88 SW-HAS-NO-INTERNALS   VALUE 'N'.                       ELGACP  
00265      02                          PICTURE  X(01).                  ELGACP  
00266         88 SW-HAS-IBGR           VALUE 'Y'.                       ELGACP  
00267         88 SW-HAS-NO-IBGR        VALUE 'N'.                       ELGACP  
00268      02                          PICTURE  X(01).                  ELGACP  
00269         88 SW-HAS-IDGD           VALUE 'Y'.                       ELGACP  
00270         88 SW-HAS-NO-IDGD        VALUE 'N'.                       ELGACP  
00271      02                          PICTURE  X(01).                  ELGACP  
00272         88 SW-HAS-IPGN           VALUE 'Y'.                       ELGACP  
00273         88 SW-HAS-NO-IPGN        VALUE 'N'.                       ELGACP  
00274      02                          PICTURE  X(01).                  ELGACP  
00275         88 SW-HAS-IPGP           VALUE 'Y'.                       ELGACP  
00276         88 SW-HAS-NO-IPGP        VALUE 'N'.                       ELGACP  
00277      02                          PICTURE  X(01).                  ELGACP  
00278         88 SW-HAS-IPGT           VALUE 'Y'.                       ELGACP  
00279         88 SW-HAS-NO-IPGT        VALUE 'N'.                       ELGACP  
00280      02                          PICTURE  X(01).                  ELGACP  
00281         88 SW-HAS-IPGS           VALUE 'Y'.                       ELGACP  
00282         88 SW-HAS-NO-IPGS        VALUE 'N'.                       ELGACP  
00283      02 WS-AA-SWITCH             PICTURE  X(01).                  ELGACP  
00284         88 AA-FOUND              VALUE 'Y'.                       ELGACP  
00285         88 AA-NOT-FOUND          VALUE 'N'.                       ELGACP  
00286                                                                   ELGACP  
00287      02 WS-ACP-SUB                PIC S9(04) COMP.                ELGACP  
00288      02 WS-COUNTER-ACP            PIC S9(04) COMP VALUE +0.       ELGACP  
00289                                                                   ELGACP  
00290  01  WS-INT-TAB-LST.                                              ELGACP  
00291      02 WS-TAB-SUB                PIC S9(04) COMP.                ELGACP  
00292      02 WS-INT-TAB                OCCURS 6 TIMES.                 ELGACP  
00293         03                        PIC  X(18).                     ELGACP  
00294            88 WS-INT-TAB-IBGR VALUE ' BENEFIT PROVISION'.         ELGACP  
00295            88 WS-INT-TAB-IDGD VALUE '         DIAGNOSIS'.         ELGACP  
00296            88 WS-INT-TAB-IPGN VALUE '   PROVIDER NUMBER'.         ELGACP  
00297            88 WS-INT-TAB-IPGP VALUE '         PROCEDURE'.         ELGACP  
00298            88 WS-INT-TAB-IPGT VALUE '     PROVIDER TYPE'.         ELGACP  
00299            88 WS-INT-TAB-IPGS VALUE 'PROVIDER SPECIALTY'.         ELGACP  
00300         03                        PIC  X(05).                     ELGACP  
00301            88 WS-INT-TAB-AND      VALUE ' AND '.                  ELGACP  
00302            88 WS-INT-TAB-COMMA    VALUE ',    '.                  ELGACP  
00303            88 WS-INT-TAB-END      VALUE SPACES.                   ELGACP  
00304                                                                   ELGACP  
00305  01  WS-INT-TAB-TXT               REDEFINES WS-INT-TAB-LST.       ELGACP  
00306      02                           PIC S9(04) COMP.                ELGACP  
00307      02 WS-INT-TAB-TXT-1          PICTURE  X(69).                 ELGACP  
00308      02 WS-INT-TAB-TXT-2          PICTURE  X(46).                 ELGACP  
00309                                                                   ELGACP  
00310  01  WS-TEXT-HOLD-AREA.                                           ELGACP  
00311      02 WS-TEXT-HOLD-COUNT       PICTURE S9(4)           COMP.    ELGACP  
00312      02 WS-TEXT-HOLD-TEXT        PICTURE  X(1580).                ELGACP  
00313                                                                   ELGACP  
00314   01  WS-MULTI-VALUE-LINE.                                        ELGACP  
00315       05  WS-MVL-CHANGES-PHRASE    PIC  X(16) VALUE SPACES.       ELGACP  
00316       05  WS-MVL-MASK              PIC  X(58) VALUE SPACES.       ELGACP  
00317       05  FILLER                   PIC  X(01) VALUE SPACES.       ELGACP  
00318       05  WS-MVL-LEVEL-TAG         PIC  X(04) VALUE SPACES.       ELGACP  
00319                                                                   ELGACP  
00320      TITLE 'LINKAGE SECTION'.                                     ELGACP  
00321  LINKAGE SECTION.                                                 ELGACP  
00322                                                                   ELGACP  
00323  01  DFHCOMMAREA.                                                 ELGACP  
00324      COPY ELSCOMMC.                                               ELGACP  
00325 /                                                                 ELGACP  
00326      COPY ELSCIA2C.                                               ELGACP  
00327 /                                                                 ELGACP  
00328      COPY ELSIOPMC.                                               ELGACP  
00329 /                                                                 ELGACP  
00330      COPY ELSCMIFC.                                               ELGACP  
00331 /                                                                 ELGACP  
00332      COPY ELSCMDSC.                                               ELGACP  
00333 /                                                                 ELGACP  
00334      COPY ELSOUTPC.                                               ELGACP  
00335 /                                                                 ELGACP  
00336      COPY ELSSRTPC.                                               ELGACP  
00337 /                                                                 ELGACP  
00338      COPY ELSSSCBC.                                               ELGACP  
00339 /                                                                 ELGACP  
00340      COPY ELSTCWAC.                                               ELGACP  
00341 /                                                                 ELGACP  
00342      COPY ELSACUMC.                                               ELGACP  
00343 /                                                                 ELGACP  
00344  01  GCG-GROUP-SPECIFIC-RECORD.                                   ELGACP  
00345      COPY GCGROUPC.                                               ELGACP  
00346 /                                                                 ELGACP  
00347      TITLE 'PROCEDURE DIVISION'.                                  ELGACP  
00348 ************************************************************      ELGACP  
00349 *                                                          *      ELGACP  
00350 *    PROCEDURE DIVISION                                    *      ELGACP  
00351 *                                                          *      ELGACP  
00352 ************************************************************      ELGACP  
00353                                                                   ELGACP  
00354  PROCEDURE DIVISION.                                              ELGACP  
00355                                                                   ELGACP  
00356      PERFORM 0010-INITIALIZATION.                                 ELGACP  
00357      PERFORM 0130-PROCESS.                                        ELGACP  
00358      GOBACK.                                                      ELGACP  
00359                                                                   ELGACP  
00360 /***********************************************************      ELGACP  
00361 *                                                          *      ELGACP  
00362 *        INITIALIZATION                                    *      ELGACP  
00363 *                                                          *      ELGACP  
00364 ************************************************************      ELGACP  
00365                                                                   ELGACP  
00366  0010-INITIALIZATION.                                             ELGACP  
00367 * -- ESTABLISH STANDARD ENVIRONMENT                               ELGACP  
00368      PERFORM 0020-EST-ADR-OF-CONTROL-BLOCKS.                      ELGACP  
00369                                                                   ELGACP  
00370 * -- ESTABLISH ADDRESSABILITY OF WORK AREAS                       ELGACP  
00371      PERFORM 0070-EST-ADR-OF-TEMPORARY-FILE.                      ELGACP  
00372      PERFORM 0080-EST-ADR-OF-CDES-MANUAL.                         ELGACP  
00373      PERFORM 0090-EST-ADR-OF-OUTPUT-INTERFA.                      ELGACP  
00374      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGACP  
00375      PERFORM 0110-EST-ADR-OF-TEXT-COMPRESSX.                      ELGACP  
00376      PERFORM 0120-EST-ADR-OF-GRP-SPC.                             ELGACP  
00377                                                                   ELGACP  
00378 * -- CLEAR OUTPUT AND INITIALIZE TEXT COMPRESSION WORK AREA       ELGACP  
00379      IF COF-NBR-DTL-LINES > 0                                     ELGACP  
00380      THEN                                                         ELGACP  
00381         CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.      ELGACP  
00382                                                                   ELGACP  
00383      INITIALIZE TCAR-FROM-AREA                                    ELGACP  
00384                 TCAR-FROM-LENGTH                                  ELGACP  
00385                 TCAR-FROM-SUB.                                    ELGACP  
00386                                                                   ELGACP  
00387 ************************************************************      ELGACP  
00388 *                                                          *      ELGACP  
00389 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGACP  
00390 *                                                          *      ELGACP  
00391 ************************************************************      ELGACP  
00392                                                                   ELGACP  
00393  0020-EST-ADR-OF-CONTROL-BLOCKS.                                  ELGACP  
00394      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGACP  
00395      THEN                                                         ELGACP  
00396         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGACP  
00397      ELSE                                                         ELGACP  
00398         IF ECA-CIA-PTR = NULL                                     ELGACP  
00399         THEN                                                      ELGACP  
00400            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGACP  
00401         ELSE                                                      ELGACP  
00402            CALL 'ELUINISM'                                        ELGACP  
00403               USING DFHCOMMAREA                                   ELGACP  
00404                     ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA      ELGACP  
00405            SET CIA-ELSSSCB-DDN TO TRUE                            ELGACP  
00406            CALL 'ELUSETAD'                                        ELGACP  
00407               USING DFHCOMMAREA                                   ELGACP  
00408                     ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK        ELGACP  
00409            IF CIA-RC-PTR-NULL                                     ELGACP  
00410            THEN                                                   ELGACP  
00411               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGACP  
00412               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGACP  
00413            ELSE                                                   ELGACP  
00414               CONTINUE                                            ELGACP  
00415            END-IF                                                 ELGACP  
00416         END-IF                                                    ELGACP  
00417      END-IF.                                                      ELGACP  
00418                                                                   ELGACP  
00419 /***********************************************************      ELGACP  
00420 *                                                          *      ELGACP  
00421 *        ESTABLISH ADDRESSABILITY OF TEMPORARY FILE        *      ELGACP  
00422 *                                                          *      ELGACP  
00423 ************************************************************      ELGACP  
00424                                                                   ELGACP  
00425  0070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGACP  
00426      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGACP  
00427      CALL 'ELUSETAD'                                              ELGACP  
00428         USING DFHCOMMAREA                                         ELGACP  
00429               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELGACP  
00430      IF CIA-RC-PTR-NULL                                           ELGACP  
00431         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACP  
00432         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGACP  
00433                                                                   ELGACP  
00434 ************************************************************      ELGACP  
00435 *                                                          *      ELGACP  
00436 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE*      ELGACP  
00437 *                                                          *      ELGACP  
00438 ************************************************************      ELGACP  
00439                                                                   ELGACP  
00440  0080-EST-ADR-OF-CDES-MANUAL.                                     ELGACP  
00441      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGACP  
00442      CALL 'ELUSETAD'                                              ELGACP  
00443         USING DFHCOMMAREA                                         ELGACP  
00444               ADDRESS OF CMF-CODES-MANUAL-INTERFACE.              ELGACP  
00445      IF CIA-RC-PTR-NULL                                           ELGACP  
00446         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACP  
00447         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGACP  
00448                                                                   ELGACP  
00449 ************************************************************      ELGACP  
00450 *                                                          *      ELGACP  
00451 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGACP  
00452 *                                                          *      ELGACP  
00453 ************************************************************      ELGACP  
00454                                                                   ELGACP  
00455  0090-EST-ADR-OF-OUTPUT-INTERFA.                                  ELGACP  
00456      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGACP  
00457      CALL 'ELUSETAD'                                              ELGACP  
00458         USING DFHCOMMAREA                                         ELGACP  
00459               ADDRESS OF COF-OUTPUT-INTERFACE.                    ELGACP  
00460      IF CIA-RC-PTR-NULL                                           ELGACP  
00461         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACP  
00462         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGACP  
00463                                                                   ELGACP  
00464 ************************************************************      ELGACP  
00465 *                                                          *      ELGACP  
00466 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGACP  
00467 *                                                          *      ELGACP  
00468 ************************************************************      ELGACP  
00469                                                                   ELGACP  
00470  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGACP  
00471      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGACP  
00472      CALL 'ELUSETAD'                                              ELGACP  
00473         USING DFHCOMMAREA                                         ELGACP  
00474               ADDRESS OF SRP-SUBROUTINE-PARAMETERS.               ELGACP  
00475      IF CIA-RC-PTR-NULL                                           ELGACP  
00476         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACP  
00477         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGACP  
00478                                                                   ELGACP  
00479 ************************************************************      ELGACP  
00480 *                                                          *      ELGACP  
00481 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELGACP  
00482 *                                                          *      ELGACP  
00483 ************************************************************      ELGACP  
00484                                                                   ELGACP  
00485  0110-EST-ADR-OF-TEXT-COMPRESSX.                                  ELGACP  
00486      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGACP  
00487      CALL 'ELUSETAD'                                              ELGACP  
00488         USING DFHCOMMAREA                                         ELGACP  
00489               ADDRESS OF TCAR-COMPRESSION-WORK-AREA.              ELGACP  
00490      IF CIA-RC-PTR-NULL                                           ELGACP  
00491         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACP  
00492         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGACP  
00493                                                                   ELGACP  
00494 ************************************************************      ELGACP  
00495 *                                                          *      ELGACP  
00496 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD *      ELGACP  
00497 *                                                          *      ELGACP  
00498 ************************************************************      ELGACP  
00499                                                                   ELGACP  
00500  0120-EST-ADR-OF-GRP-SPC.                                         ELGACP  
00501      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGACP  
00502      CALL 'ELUSETAD'                                              ELGACP  
00503         USING DFHCOMMAREA                                         ELGACP  
00504               ADDRESS OF GCG-GROUP-SPECIFIC-RECORD.               ELGACP  
00505      IF CIA-RC-PTR-NULL                                           ELGACP  
00506         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGACP  
00507         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGACP  
00508                                                                   ELGACP  
00509 /***********************************************************      ELGACP  
00510 *                                                          *      ELGACP  
00511 *        PROCESS                                           *      ELGACP  
00512 *                                                          *      ELGACP  
00513 ************************************************************      ELGACP  
00514                                                                   ELGACP  
00515  0130-PROCESS.                                                    ELGACP  
00516 * -- GENERATE INITIAL HEADER                                      ELGACP  
00517      EVALUATE TRUE                                                ELGACP  
00518          WHEN SRP-TOPIC-ACCUM                                     ELGACP  
00519               PERFORM 0140-OBTAIN-TOPIC-HEADER                    ELGACP  
00520          WHEN SRP-BEN-PROV-ACCUM                                  ELGACP  
00521               PERFORM 0150-OBTAIN-BEN-PROVN-HEADER                ELGACP  
00522      END-EVALUATE.                                                ELGACP  
00523                                                                   ELGACP  
00524 * -- GENERATE ACCUMULATOR OUTPUT                                  ELGACP  
00525      EVALUATE TRUE                                                ELGACP  
00526         WHEN SRP-NO-ACCUMS-FOUND                                  ELGACP  
00527            PERFORM 0170-DSPLY-NO-ACCUMS                           ELGACP  
00528         WHEN SRP-INST-NOT-APPLICABLE                              ELGACP  
00529            PERFORM 0180-DSPLY-INST-NOT-APPLIC                     ELGACP  
00530         WHEN SRP-PROF-NOT-APPLICABLE                              ELGACP  
00531            PERFORM 0190-DSPLY-PROF-NOT-APPLIC                     ELGACP  
00532         WHEN OTHER                                                ELGACP  
00533            PERFORM 0200-DISPLAY-REGULAR-TEXT                      ELGACP  
00534      END-EVALUATE.                                                ELGACP  
00535      IF SRP-TOPIC-ACCUM                                           ELGACP  
00536         PERFORM 0680-END-THE-DISPLAY.                             ELGACP  
00537                                                                   ELGACP  
00538 /***********************************************************      ELGACP  
00539 *                                                          *      ELGACP  
00540 *        OBTAIN TOPIC HEADER                               *      ELGACP  
00541 *                                                          *      ELGACP  
00542 ************************************************************      ELGACP  
00543                                                                   ELGACP  
00544  0140-OBTAIN-TOPIC-HEADER.                                        ELGACP  
00545      MOVE +2            TO  COF-NBR-HDR-LINES.                    ELGACP  
00546      MOVE HD-TOPIC-HDR  TO  COF-HDR-LINE (2).                     ELGACP  
00547      MOVE  0            TO  COF-NBR-DTL-LINES.                    ELGACP  
00548      SET  COF-NEW-PAGE  TO  TRUE.                                 ELGACP  
00549      PERFORM 0160-SEND-INITL-HDR.                                 ELGACP  
00550                                                                   ELGACP  
00551 ************************************************************      ELGACP  
00552 *                                                          *      ELGACP  
00553 *        OBTAIN BENEFIT PROVISION HEADER                   *      ELGACP  
00554 *                                                          *      ELGACP  
00555 ************************************************************      ELGACP  
00556                                                                   ELGACP  
00557  0150-OBTAIN-BEN-PROVN-HEADER.                                    ELGACP  
00558      INITIALIZE COF-DTL-LINE (1).                                 ELGACP  
00559      MOVE HD-BEN-PROVN-HDR TO COF-DTL-LINE (2).                   ELGACP  
00560      SET  COF-CONTINUE  TO  TRUE.                                 ELGACP  
00561      MOVE +0            TO  COF-NBR-HDR-LINES.                    ELGACP  
00562      MOVE +2            TO  COF-NBR-DTL-LINES.                    ELGACP  
00563      PERFORM 0160-SEND-INITL-HDR.                                 ELGACP  
00564                                                                   ELGACP  
00565 ************************************************************      ELGACP  
00566 *                                                          *      ELGACP  
00567 *    SEND INITIAL HEADER                                   *      ELGACP  
00568 *                                                          *      ELGACP  
00569 ************************************************************      ELGACP  
00570                                                                   ELGACP  
00571  0160-SEND-INITL-HDR.                                             ELGACP  
00572      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGACP  
00573      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGACP  
00574      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACP  
00575      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGACP  
00576                                                                   ELGACP  
00577 /***********************************************************      ELGACP  
00578 *                                                          *      ELGACP  
00579 *        DISPLAY NO ACCUMS MESSAGE                         *      ELGACP  
00580 *                                                          *      ELGACP  
00581 ************************************************************      ELGACP  
00582                                                                   ELGACP  
00583  0170-DSPLY-NO-ACCUMS.                                            ELGACP  
00584      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00585      MOVE PH-NO-ACP TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGACP  
00586      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
00587                                                                   ELGACP  
00588 ************************************************************      ELGACP  
00589 *                                                          *      ELGACP  
00590 *    DISPLAY INSTITUTIONAL COPAY NOT APPLICABLE            *      ELGACP  
00591 *                                                          *      ELGACP  
00592 ************************************************************      ELGACP  
00593                                                                   ELGACP  
00594  0180-DSPLY-INST-NOT-APPLIC.                                      ELGACP  
00595      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00596      MOVE PH-NO-INST-ACP TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGACP  
00597      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
00598                                                                   ELGACP  
00599 ************************************************************      ELGACP  
00600 *                                                          *      ELGACP  
00601 *    DISPLAY PROFESSIONAL COPAYS NOT APPLICABLE            *      ELGACP  
00602 *                                                          *      ELGACP  
00603 ************************************************************      ELGACP  
00604                                                                   ELGACP  
00605  0190-DSPLY-PROF-NOT-APPLIC.                                      ELGACP  
00606      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00607      MOVE PH-NO-PROF-ACP TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGACP  
00608      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
00609                                                                   ELGACP  
00610 /***********************************************************      ELGACP  
00611 *                                                          *      ELGACP  
00612 *        DISPLAY REGULAR TEXT                              *      ELGACP  
00613 *                                                          *      ELGACP  
00614 ************************************************************      ELGACP  
00615                                                                   ELGACP  
00616  0200-DISPLAY-REGULAR-TEXT.                                       ELGACP  
00617 * -- DO INITIAL READ                                              ELGACP  
00618      MOVE 1 TO IOP-TSQ-ITEM-NBR.                                  ELGACP  
00619      SET  IOP-RD  TO  TRUE.                                       ELGACP  
00620      PERFORM 0660-READ-ACCUM-WORK-FILE-REC.                       ELGACP  
00621                                                                   ELGACP  
00622 * -- PROCESS UNTIL DONE                                           ELGACP  
00623      PERFORM 0210-DISPLAY-OCCURENCE-TEXT                          ELGACP  
00624          UNTIL NOT IOP-RC-OK.                                     ELGACP  
00625                                                                   ELGACP  
00626 * -- DELETE WORK FILE WHEN DONE                                   ELGACP  
00627      PERFORM 0670-DELETE-ACCUM-WORK-FILE.                         ELGACP  
00628                                                                   ELGACP  
00629 /***********************************************************      ELGACP  
00630 *                                                          *      ELGACP  
00631 *        DISPLAY OCCURENCE TEXT                            *      ELGACP  
00632 *                                                          *      ELGACP  
00633 ************************************************************      ELGACP  
00634                                                                   ELGACP  
00635  0210-DISPLAY-OCCURENCE-TEXT.                                     ELGACP  
00636                                                                   ELGACP  
00637      PERFORM 0220-INIT-OCCRNC-PROC.                               ELGACP  
00638                                                                   ELGACP  
00639      PERFORM 0230-DISPLAY-COMMON-BODY-TEXT.                       ELGACP  
00640                                                                   ELGACP  
00641      SET IOP-RD-NXT  TO  TRUE.                                    ELGACP  
00642      PERFORM 0660-READ-ACCUM-WORK-FILE-REC.                       ELGACP  
00643                                                                   ELGACP  
00644      EVALUATE TRUE                                                ELGACP  
00645          WHEN SRP-TOPIC-ACCUM                                     ELGACP  
00646               PERFORM 0520-GEN-TOPIC-TRAILER                      ELGACP  
00647               IF IOP-RC-OK                                        ELGACP  
00648                  THEN                                             ELGACP  
00649                     PERFORM 0140-OBTAIN-TOPIC-HEADER              ELGACP  
00650                  ELSE                                             ELGACP  
00651                     CONTINUE                                      ELGACP  
00652               END-IF                                              ELGACP  
00653          WHEN SRP-BEN-PROV-ACCUM                                  ELGACP  
00654               PERFORM 0530-GEN-BP-TRAILER                         ELGACP  
00655      END-EVALUATE.                                                ELGACP  
00656                                                                   ELGACP  
00657 ************************************************************      ELGACP  
00658 *                                                          *      ELGACP  
00659 *    INITIALIZE OCCURRENCE PROCESSING                      *      ELGACP  
00660 *                                                          *      ELGACP  
00661 ************************************************************      ELGACP  
00662  0220-INIT-OCCRNC-PROC.                                           ELGACP  
00663      SET SW-HAS-NO-INTERNALS                                      ELGACP  
00664          SW-HAS-NO-IBGR                                           ELGACP  
00665          SW-HAS-NO-IDGD                                           ELGACP  
00666          SW-HAS-NO-IPGN                                           ELGACP  
00667          SW-HAS-NO-IPGP                                           ELGACP  
00668          SW-HAS-NO-IPGT                                           ELGACP  
00669          SW-HAS-NO-IPGS                                           ELGACP  
00670       TO TRUE.                                                    ELGACP  
00671                                                                   ELGACP  
00672 /***********************************************************      ELGACP  
00673 *                                                          *      ELGACP  
00674 *        DISPLAY COMMON BODY TEXT                          *      ELGACP  
00675 *                                                          *      ELGACP  
00676 ************************************************************      ELGACP  
00677                                                                   ELGACP  
00678  0230-DISPLAY-COMMON-BODY-TEXT.                                   ELGACP  
00679                                                                   ELGACP  
00680      PERFORM 0240-CHK-INTRNL-TABS.                                ELGACP  
00681                                                                   ELGACP  
00682      PERFORM 0250-GEN-ACP-VALUE-SENT.                             ELGACP  
00683                                                                   ELGACP  
00684      PERFORM 0350-GEN-APPLIC-SENT-1.                              ELGACP  
00685                                                                   ELGACP  
00686      PERFORM 0370-GEN-APPLIC-SENT-2.                              ELGACP  
00687                                                                   ELGACP  
00688      IF DEFINITION-NA                                             ELGACP  
00689      THEN                                                         ELGACP  
00690         CONTINUE                                                  ELGACP  
00691      ELSE                                                         ELGACP  
00692         PERFORM 0510-GEN-DEFN-SENT.                               ELGACP  
00693                                                                   ELGACP  
00694      SET ASC-DES-INDEX TO 1.                                      ELGACP  
00695      PERFORM 0650-GEN-INTRNL-TAB-LSTNGS.                          ELGACP  
00696                                                                   ELGACP  
00697 /***********************************************************      ELGACP  
00698 *                                                          *      ELGACP  
00699 *    CHECK FOR INTERNAL TABULARS                           *      ELGACP  
00700 *                                                          *      ELGACP  
00701 ************************************************************      ELGACP  
00702  0240-CHK-INTRNL-TABS.                                            ELGACP  
00703      IF NO-IBGR-SLOT-NBR (1)                                      ELGACP  
00704      THEN CONTINUE                                                ELGACP  
00705      ELSE SET SW-HAS-IBGR SW-HAS-INTERNALS TO TRUE.               ELGACP  
00706                                                                   ELGACP  
00707      IF NO-IDGD-SLOT-NBR (1)                                      ELGACP  
00708      THEN CONTINUE                                                ELGACP  
00709      ELSE SET SW-HAS-IDGD SW-HAS-INTERNALS TO TRUE.               ELGACP  
00710                                                                   ELGACP  
00711      IF NO-IPGN-SLOT-NBR (1)                                      ELGACP  
00712      THEN CONTINUE                                                ELGACP  
00713      ELSE SET SW-HAS-IPGN SW-HAS-INTERNALS TO TRUE.               ELGACP  
00714                                                                   ELGACP  
00715      IF NO-IPGP-SLOT-NBR (1)                                      ELGACP  
00716      THEN CONTINUE                                                ELGACP  
00717      ELSE SET SW-HAS-IPGP SW-HAS-INTERNALS TO TRUE.               ELGACP  
00718                                                                   ELGACP  
00719      IF NO-IPGT-SLOT-NBR (1)                                      ELGACP  
00720      THEN CONTINUE                                                ELGACP  
00721      ELSE SET SW-HAS-IPGT SW-HAS-INTERNALS TO TRUE.               ELGACP  
00722                                                                   ELGACP  
00723      IF NO-IPGS-SLOT-NBR (1)                                      ELGACP  
00724      THEN CONTINUE                                                ELGACP  
00725      ELSE SET SW-HAS-IPGS SW-HAS-INTERNALS TO TRUE.               ELGACP  
00726                                                                   ELGACP  
00727 /***********************************************************      ELGACP  
00728 *                                                          *      ELGACP  
00729 *        GENERATE COPAY DESCRIPTION                        *      ELGACP  
00730 *                                                          *      ELGACP  
00731 ************************************************************      ELGACP  
00732  0250-GEN-ACP-VALUE-SENT.                                         ELGACP  
00733                                                                   ELGACP  
00734      SET COPAY-INDEX TO 1.                                        ELGACP  
00735      MOVE 1 TO WS-ACP-SUB.                                        ELGACP  
00736      IF ACCUM-COPAY-DEFINITION (COPAY-INDEX)                      ELGACP  
00737               = '0A' OR '0B' OR '0C'                              ELGACP  
00738         SET COPAY-INDEX TO 1                                      ELGACP  
00739         PERFORM 0255-LEAD-IN-SENTENCE                             ELGACP  
00740         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
00741         MOVE ACCUM-COPAY-VALUE-LIMIT (COPAY-INDEX)                ELGACP  
00742            TO  PH-ACP-VAL-LMT-DOLLARS                             ELGACP  
00743         MOVE PH-ACP-VAL-LMT-DOLLARS TO                            ELGACP  
00744            TCAR-FROM-LINE (TCAR-FROM-SUB)                         ELGACP  
00745         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
00746         PERFORM 0601-SELECT-BEN-PER-TRANS                         ELGACP  
00747         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
00748         SET COPAY-INDEX UP BY 1                                   ELGACP  
00749         PERFORM 0600-PAIRED-DISPLAY                               ELGACP  
00750 *       IF AA-FOUND                                               ELGACP  
00751 *          PERFORM 0605-FINISH-SENTENCE                           ELGACP  
00752 *       ELSE                                                      ELGACP  
00753 *          PERFORM 0275-FIND-AA-OCCURS                            ELGACP  
00754 *       END-IF                                                    ELGACP  
00755      ELSE                                                         ELGACP  
00756 *    IF COPAY-INDEX = 1                                           ELGACP  
00757 *       SET COPAY-INDEX TO 2                                      ELGACP  
00758 *       PERFORM 0625-DISPLAY-LEADIN-PHRASE                        ELGACP  
00759 *       PERFORM 0910-COMPLETE-AND-SEND-PARA                       ELGACP  
00760 *    ELSE                                                         ELGACP  
00761         PERFORM 0650-SIMPLE-COPAY                                 ELGACP  
00762      END-IF.                                                      ELGACP  
00763                                                                   ELGACP  
00764      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
00765                                                                   ELGACP  
00766 /***********************************************************      ELGACP  
00767 *                                                          *      ELGACP  
00768 *    LEAD IN SENTENCE                                      *      ELGACP  
00769 *                                                          *      ELGACP  
00770 ************************************************************      ELGACP  
00771  0255-LEAD-IN-SENTENCE.                                           ELGACP  
00772      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00773      MOVE 1 TO TCAR-FROM-SUB.                                     ELGACP  
00774      MOVE PH-APPLIC-1-LEAD-C TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGACP  
00775                                                                   ELGACP  
00776      MOVE ACCUM-FAM-OR-INDIV   TO  CMF-CODE-VALUE.                ELGACP  
00777      MOVE 'COPAY-FAM-OR-INDIV' TO  CMF-ELEMENT-SYSTEM-NAME.       ELGACP  
00778      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
00779                                                                   ELGACP  
00780 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00781 *    MOVE PH-WD-FOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGACP  
00782                                                                   ELGACP  
00783 *    MOVE ACCUM-L-O-B   TO  CMF-CODE-VALUE.                       ELGACP  
00784 *    MOVE 'COPAY-L-O-B' TO  CMF-ELEMENT-SYSTEM-NAME.              ELGACP  
00785 *    PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
00786                                                                   ELGACP  
00787      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00788      MOVE WS-AS-FOLLOWS  TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGACP  
00789      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00790      MOVE WS-COLON       TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGACP  
00791                                                                   ELGACP  
00792 ************************************************************      ELGACP  
00793 *    FIND THE AA OCCURANCE                                 *      ELGACP  
00794 *                                                          *      ELGACP  
00795 ************************************************************      ELGACP  
00796 *0275-FIND-AA-OCCURS.                                             ELGACP  
00797 *    SET WS-INDEX TO 1.                                           ELGACP  
00798 *    PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL                   ELGACP  
00799 *      > ACCUM-COPAY-COUNT OR (AA-FOUND)                          ELGACP  
00800 *         IF ACCUM-COPAY-DEFINITION (COPAY-INDEX) = 'AA'          ELGACP  
00801 *             SET AA-FOUND TO TRUE                                ELGACP  
00802 *             SET WS-INDEX TO WS-AA-INDEX                         ELGACP  
00803 *         END-IF                                                  ELGACP  
00804 *    END-PERFORM.                                                 ELGACP  
00805                                                                   ELGACP  
00806 ************************************************************      ELGACP  
00807 *    GENERATE STANDARD COPAY PHRASE                        *      ELGACP  
00808 *                                                          *      ELGACP  
00809 ************************************************************      ELGACP  
00810  0270-GEN-STD-ACP-PH.                                             ELGACP  
00811      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00812      MOVE PH-WD-OF TO TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGACP  
00813                                                                   ELGACP  
00814      IF ACCUM-VAL-UNLIM (1)                                       ELGACP  
00815      THEN                                                         ELGACP  
00816         PERFORM 0280-GEN-UNLIM-TERM                               ELGACP  
00817      ELSE                                                         ELGACP  
00818         IF ACCUM-VALUE-QUALIFIER = '5'                            ELGACP  
00819         THEN                                                      ELGACP  
00820            PERFORM 0290-GEN-DOLLAR-LIM-TERM                       ELGACP  
00821         ELSE                                                      ELGACP  
00822            PERFORM 0300-GEN-OTHER-LIM-TERM                        ELGACP  
00823         END-IF                                                    ELGACP  
00824      END-IF.                                                      ELGACP  
00825      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00826      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGACP  
00827                                                                   ELGACP  
00828 ************************************************************      ELGACP  
00829 *                                                          *      ELGACP  
00830 *    GENERATE UNLIMITED COPAY TERM                         *      ELGACP  
00831 *                                                          *      ELGACP  
00832 ************************************************************      ELGACP  
00833  0280-GEN-UNLIM-TERM.                                             ELGACP  
00834      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00835      MOVE PH-WD-UNLIMITED TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELGACP  
00836      MOVE 'COPAY-VALUE-QUALIFIER' TO CMF-ELEMENT-SYSTEM-NAME.     ELGACP  
00837      MOVE ACCUM-VALUE-QUALIFIER TO CMF-CODE-VALUE                 ELGACP  
00838      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
00839                                                                   ELGACP  
00840 ************************************************************      ELGACP  
00841 *                                                          *      ELGACP  
00842 *    GENERATE DOLLAR COPAY TERM                            *      ELGACP  
00843 *                                                          *      ELGACP  
00844 ************************************************************      ELGACP  
00845  0290-GEN-DOLLAR-LIM-TERM.                                        ELGACP  
00846      MOVE ACCUM-VALUE-LIMIT (1) TO PH-ACP-VAL-LMT-DOLLARS.        ELGACP  
00847      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00848      MOVE PH-ACP-VAL-LMT-DOLLARS                                  ELGACP  
00849        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACP  
00850                                                                   ELGACP  
00851 ************************************************************      ELGACP  
00852 *                                                          *      ELGACP  
00853 *    GENERATE OTHER COPAY TERM                             *      ELGACP  
00854 *                                                          *      ELGACP  
00855 ************************************************************      ELGACP  
00856  0300-GEN-OTHER-LIM-TERM.                                         ELGACP  
00857      MOVE ACCUM-VALUE-LIMIT-NON-DOLLAR (1)                        ELGACP  
00858                       TO PH-ACP-VAL-LMT-OTHER                     ELGACP  
00859      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00860      MOVE PH-ACP-VAL-LMT-OTHER TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGACP  
00861                                                                   ELGACP  
00862      MOVE ACCUM-VALUE-QUALIFIER TO CMF-CODE-VALUE                 ELGACP  
00863      MOVE 'COPAY-VALUE-QUALIFIER' TO CMF-ELEMENT-SYSTEM-NAME.     ELGACP  
00864      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
00865                                                                   ELGACP  
00866 /***********************************************************      ELGACP  
00867 *                                                          *      ELGACP  
00868 *    GENERATE BENEFIT PERIOD PHRASE                        *      ELGACP  
00869 *                                                          *      ELGACP  
00870 ************************************************************      ELGACP  
00871  0310-GEN-BEN-PER-PH.                                             ELGACP  
00872 * -- CLEAR COMPRESSION INPUT BUFFER                               ELGACP  
00873      IF TCAR-FROM-SUB > 1                                         ELGACP  
00874      THEN                                                         ELGACP  
00875         PERFORM 0600-SEND-PART-PARA.                              ELGACP  
00876                                                                   ELGACP  
00877      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00878      MOVE PH-WD-PER TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGACP  
00879                                                                   ELGACP  
00880      MOVE ACCUM-BENEFIT-PERIOD   TO  CMF-CODE-VALUE.              ELGACP  
00881      MOVE 'COPAY-BENEFIT-PERIOD' TO  CMF-ELEMENT-SYSTEM-NAME.     ELGACP  
00882      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
00883                                                                   ELGACP  
00884                                                                   ELGACP  
00885      IF ACCUM-BEN-PER-TIME-QUAL NOT = ZEROS                       ELGACP  
00886      THEN                                                         ELGACP  
00887         PERFORM 0320-GEN-BEN-PER-TIME-FCTR-TRM.                   ELGACP  
00888                                                                   ELGACP  
00889      IF ACCUM-INTERVAL-TIME-FCTR NOT = ZEROS                      ELGACP  
00890      THEN                                                         ELGACP  
00891         PERFORM 0330-GEN-INTRVL-TIME-FCTR-TRM.                    ELGACP  
00892                                                                   ELGACP  
00893      IF INTERVAL-OVRD-IND-NA                                      ELGACP  
00894      THEN                                                         ELGACP  
00895         CONTINUE                                                  ELGACP  
00896      ELSE                                                         ELGACP  
00897         PERFORM 0340-GEN-INTRVL-OVRD-NOTE.                        ELGACP  
00898                                                                   ELGACP  
00899      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00900      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGACP  
00901                                                                   ELGACP  
00902 /***********************************************************      ELGACP  
00903 *                                                          *      ELGACP  
00904 *    GENERATE CARRY OVER CREDIT PHR                        *      ELGACP  
00905 *                                                          *      ELGACP  
00906 ************************************************************      ELGACP  
00907  0315-GEN-CARRY-OVER-PHR.                                         ELGACP  
00908      IF TCAR-FROM-SUB > 1                                         ELGACP  
00909         PERFORM 0600-SEND-PART-PARA.                              ELGACP  
00910      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00911      MOVE PH-CARRY-OVER TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELGACP  
00912                                                                   ELGACP  
00913      MOVE 'CARRY-OVER-CREDIT-IND'     TO CMF-ELEMENT-SYSTEM-NAME. ELGACP  
00914      MOVE ACCUM-CARRY-OVER-CREDIT-IND TO CMF-CODE-VALUE.          ELGACP  
00915      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
00916                                                                   ELGACP  
00917      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00918      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGACP  
00919                                                                   ELGACP  
00920 /***********************************************************      ELGACP  
00921 *                                                          *      ELGACP  
00922 *    GENERATE BENEFIT PERIOD TIME FACTOR TERM              *      ELGACP  
00923 *                                                          *      ELGACP  
00924 ************************************************************      ELGACP  
00925  0320-GEN-BEN-PER-TIME-FCTR-TRM.                                  ELGACP  
00926      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00927      MOVE PH-TIME-FCTR-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELGACP  
00928                                                                   ELGACP  
00929      MOVE ACCUM-BEN-PER-TIME-FCTR TO PH-BEN-PER-TIME-FCTR.        ELGACP  
00930      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00931      MOVE PH-BEN-PER-TIME-FCTR TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGACP  
00932                                                                   ELGACP  
00933      MOVE 'COPAY-BEN-PER-TIME-QUAL' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGACP  
00934      MOVE ACCUM-BEN-PER-TIME-QUAL   TO  CMF-CODE-VALUE.           ELGACP  
00935      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
00936                                                                   ELGACP  
00937 ************************************************************      ELGACP  
00938 *                                                          *      ELGACP  
00939 *    GENERATE INTERVAL TIME FACTOR TERM                    *      ELGACP  
00940 *                                                          *      ELGACP  
00941 ************************************************************      ELGACP  
00942  0330-GEN-INTRVL-TIME-FCTR-TRM.                                   ELGACP  
00943      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00944      MOVE PH-TIME-INTRVL-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGACP  
00945                                                                   ELGACP  
00946      MOVE ACCUM-INTERVAL-TIME-FCTR TO PH-INTRVL-TIME-FCTR.        ELGACP  
00947      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00948      MOVE PH-INTRVL-TIME-FCTR TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGACP  
00949                                                                   ELGACP  
00950      MOVE ACCUM-INTERVAL-TYPE   TO  CMF-CODE-VALUE.               ELGACP  
00951      MOVE 'COPAY-INTERVAL-TYPE' TO  CMF-ELEMENT-SYSTEM-NAME.      ELGACP  
00952      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
00953                                                                   ELGACP  
00954 /***********************************************************      ELGACP  
00955 *                                                          *      ELGACP  
00956 *    GENERATE INTERVAL OVERRIDE NOTE                       *      ELGACP  
00957 *                                                          *      ELGACP  
00958 ************************************************************      ELGACP  
00959  0340-GEN-INTRVL-OVRD-NOTE.                                       ELGACP  
00960      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00961      MOVE PH-INTRVL-OVRD-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGACP  
00962                                                                   ELGACP  
00963      EVALUATE ACCUM-INTERVAL-OVRD-IND                             ELGACP  
00964         WHEN '1'   PERFORM 0341-GEN-INTRVL-OVRD-SPCL-PH-1         ELGACP  
00965         WHEN '2'   PERFORM 0342-GEN-INTRVL-OVRD-SPCL-PH-2         ELGACP  
00966         WHEN '3'   PERFORM 0343-GEN-INTRVL-OVRD-SPCL-PH-3         ELGACP  
00967         WHEN OTHER                                                ELGACP  
00968            MOVE ACCUM-INTERVAL-OVRD-IND TO CMF-CODE-VALUE         ELGACP  
00969            MOVE 'COPAY-INTERVAL-OVRD-IND'                         ELGACP  
00970              TO CMF-ELEMENT-SYSTEM-NAME                           ELGACP  
00971            PERFORM 0540-XLAT-ACP-CODE-VAL                         ELGACP  
00972         END-EVALUATE.                                             ELGACP  
00973                                                                   ELGACP  
00974      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00975      MOVE PH-INTRVL-OVRD-TRAIL TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGACP  
00976                                                                   ELGACP  
00977 ************************************************************      ELGACP  
00978 *                                                          *      ELGACP  
00979 *    GENERATE INTERVAL OVERRIDE SPECIAL PHRASE 1           *      ELGACP  
00980 *                                                          *      ELGACP  
00981 ************************************************************      ELGACP  
00982  0341-GEN-INTRVL-OVRD-SPCL-PH-1.                                  ELGACP  
00983      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00984      MOVE PH-INTRVL-OVRD-SPCL-1-LEAD                              ELGACP  
00985        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACP  
00986                                                                   ELGACP  
00987      MOVE ACCUM-INTERVAL-OVRD-VALUE TO PH-INTRVL-OVRD-VAL.        ELGACP  
00988      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00989      MOVE PH-INTRVL-OVRD-VAL TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGACP  
00990                                                                   ELGACP  
00991      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
00992      MOVE PH-INTRVL-OVRD-SPCL-1-TRAIL                             ELGACP  
00993        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACP  
00994                                                                   ELGACP  
00995 ************************************************************      ELGACP  
00996 *                                                          *      ELGACP  
00997 *    GENERATE INTERVAL OVERRIDE SPECIAL PHRASE 2           *      ELGACP  
00998 *                                                          *      ELGACP  
00999 ************************************************************      ELGACP  
01000  0342-GEN-INTRVL-OVRD-SPCL-PH-2.                                  ELGACP  
01001      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01002      MOVE PH-INTRVL-OVRD-SPCL-2-LEAD                              ELGACP  
01003        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACP  
01004                                                                   ELGACP  
01005      MOVE ACCUM-INTERVAL-OVRD-VALUE TO PH-INTRVL-OVRD-VAL.        ELGACP  
01006      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01007      MOVE PH-INTRVL-OVRD-VAL TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGACP  
01008                                                                   ELGACP  
01009      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01010      MOVE PH-INTRVL-OVRD-SPCL-2-TRAIL                             ELGACP  
01011        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACP  
01012                                                                   ELGACP  
01013 ************************************************************      ELGACP  
01014 *                                                          *      ELGACP  
01015 *    GENERATE INTERVAL OVERRIDE SPECIAL PHRASE 3           *      ELGACP  
01016 *                                                          *      ELGACP  
01017 ************************************************************      ELGACP  
01018  0343-GEN-INTRVL-OVRD-SPCL-PH-3.                                  ELGACP  
01019      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01020      MOVE PH-INTRVL-OVRD-SPCL-3-LEAD                              ELGACP  
01021        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACP  
01022                                                                   ELGACP  
01023      MOVE ACCUM-INTERVAL-OVRD-VALUE TO PH-INTRVL-OVRD-VAL.        ELGACP  
01024      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01025      MOVE PH-INTRVL-OVRD-VAL TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGACP  
01026                                                                   ELGACP  
01027      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01028      MOVE PH-INTRVL-OVRD-SPCL-3-TRAIL                             ELGACP  
01029        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGACP  
01030                                                                   ELGACP  
01031 /***********************************************************      ELGACP  
01032 *                                                          *      ELGACP  
01033 *    GENERATE APPLICABILITY SENTENCE 1                     *      ELGACP  
01034 *                                                          *      ELGACP  
01035 ************************************************************      ELGACP  
01036  0350-GEN-APPLIC-SENT-1.                                          ELGACP  
01037 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01038 *    MOVE PH-APPLIC-1-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGACP  
01039                                                                   ELGACP  
01040 *    MOVE ACCUM-FAM-OR-INDIV   TO  CMF-CODE-VALUE.                ELGACP  
01041 *    MOVE 'COPAY-FAM-OR-INDIV' TO  CMF-ELEMENT-SYSTEM-NAME.       ELGACP  
01042 *    PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01043                                                                   ELGACP  
01044 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01045 *    MOVE PH-WD-FOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGACP  
01046                                                                   ELGACP  
01047 *    MOVE ACCUM-L-O-B   TO  CMF-CODE-VALUE.                       ELGACP  
01048 *    MOVE 'COPAY-L-O-B' TO  CMF-ELEMENT-SYSTEM-NAME.              ELGACP  
01049 *    PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01050                                                                   ELGACP  
01051 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01052 *    MOVE PH-WD-BENEFITS TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGACP  
01053                                                                   ELGACP  
01054      IF REINSTATEMENT-IND-NA                                      ELGACP  
01055         CONTINUE                                                  ELGACP  
01056      ELSE                                                         ELGACP  
01057         PERFORM 0360-GEN-REINST-PH                                ELGACP  
01058         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
01059         MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB)              ELGACP  
01060         PERFORM 0610-COMPLETE-AND-SEND-PARA                       ELGACP  
01061                                            .                      ELGACP  
01062 ************************************************************      ELGACP  
01063 *                                                          *      ELGACP  
01064 *    GENERATE REINSTATEMENT PHRASE                         *      ELGACP  
01065 *                                                          *      ELGACP  
01066 ************************************************************      ELGACP  
01067  0360-GEN-REINST-PH.                                              ELGACP  
01068      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01069      MOVE PH-REINST-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGACP  
01070                                                                   ELGACP  
01071      MOVE ACCUM-REINSTATEMENT-IND TO CMF-CODE-VALUE.              ELGACP  
01072      MOVE 'COPAY-REINSTATEMENT-IND' TO CMF-ELEMENT-SYSTEM-NAME.   ELGACP  
01073      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01074                                                                   ELGACP  
01075 /***********************************************************      ELGACP  
01076 *                                                          *      ELGACP  
01077 *    GENERATE APPLICABILITY SENTENCE 2                     *      ELGACP  
01078 *                                                          *      ELGACP  
01079 ************************************************************      ELGACP  
01080  0370-GEN-APPLIC-SENT-2.                                          ELGACP  
01081      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01082      MOVE PH-WD-THIS TO TCAR-FROM-LINE (TCAR-FROM-SUB).           ELGACP  
01083                                                                   ELGACP  
01084      IF FYI-VALUE-NA                                              ELGACP  
01085      THEN                                                         ELGACP  
01086         CONTINUE                                                  ELGACP  
01087      ELSE                                                         ELGACP  
01088         PERFORM 0380-GEN-FYI-PH.                                  ELGACP  
01089                                                                   ELGACP  
01090      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01091      MOVE PH-APPLIC-2-LINK TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGACP  
01092                                                                   ELGACP  
01093      IF COST-CONTAIN-IND-NA                                       ELGACP  
01094      THEN                                                         ELGACP  
01095         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
01096         MOVE PH-WD-SERVICES TO TCAR-FROM-LINE (TCAR-FROM-SUB)     ELGACP  
01097      ELSE                                                         ELGACP  
01098         PERFORM 0390-GEN-COST-CONTAINMENT-PH.                     ELGACP  
01099                                                                   ELGACP  
01100      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01101      MOVE PH-WD-FOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGACP  
01102                                                                   ELGACP  
01103      PERFORM 0400-GEN-COND-BIT-PH.                                ELGACP  
01104                                                                   ELGACP  
01105      PERFORM 0410-GEN-PLC-OF-TRTMT-PH.                            ELGACP  
01106                                                                   ELGACP  
01107      EVALUATE      AGE-LMT-FROM-IND-NA                            ELGACP  
01108               ALSO AGE-LMT-TO-IND-NA                              ELGACP  
01109               ALSO RELATIONSHIP-IND-NA                            ELGACP  
01110         WHEN TRUE ALSO TRUE ALSO TRUE                             ELGACP  
01111            CONTINUE                                               ELGACP  
01112         WHEN TRUE ALSO TRUE ALSO FALSE                            ELGACP  
01113            PERFORM 0420-GEN-PT-RLTNSHP-PH                         ELGACP  
01114         WHEN OTHER                                                ELGACP  
01115            PERFORM 0430-GEN-PT-RLTNSHP-AGE-PH                     ELGACP  
01116         END-EVALUATE.                                             ELGACP  
01117                                                                   ELGACP  
01118      IF SW-HAS-INTERNALS                                          ELGACP  
01119      THEN                                                         ELGACP  
01120         PERFORM 0460-GEN-INTRNL-TAB-LST                           ELGACP  
01121      ELSE                                                         ELGACP  
01122         CONTINUE.                                                 ELGACP  
01123                                                                   ELGACP  
01124      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01125      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGACP  
01126                                                                   ELGACP  
01127      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
01128                                                                   ELGACP  
01129 /***********************************************************      ELGACP  
01130 *                                                          *      ELGACP  
01131 *    GENERATE FYI PHRASE                                   *      ELGACP  
01132 *                                                          *      ELGACP  
01133 ************************************************************      ELGACP  
01134  0380-GEN-FYI-PH.                                                 ELGACP  
01135      MOVE ACCUM-FYI-VALUE   TO  CMF-CODE-VALUE.                   ELGACP  
01136      MOVE 'COPAY-FYI-VALUE' TO  CMF-ELEMENT-SYSTEM-NAME.          ELGACP  
01137      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01138                                                                   ELGACP  
01139 ************************************************************      ELGACP  
01140 *                                                          *      ELGACP  
01141 *    GENERATE COST CONTAINMENT PHRASE                      *      ELGACP  
01142 *                                                          *      ELGACP  
01143 ************************************************************      ELGACP  
01144  0390-GEN-COST-CONTAINMENT-PH.                                    ELGACP  
01145      MOVE ACCUM-COST-CONTAIN-IND   TO  CMF-CODE-VALUE.            ELGACP  
01146      MOVE 'COPAY-COST-CONTAIN-IND' TO  CMF-ELEMENT-SYSTEM-NAME.   ELGACP  
01147      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01148                                                                   ELGACP  
01149 ************************************************************      ELGACP  
01150 *                                                          *      ELGACP  
01151 *    GENERATE CONDITION BITS PHRASE                        *      ELGACP  
01152 *                                                          *      ELGACP  
01153 ************************************************************      ELGACP  
01154  0400-GEN-COND-BIT-PH.                                            ELGACP  
01155                                                                   ELGACP  
01156 * -- PRESERVE CONTENTS OF TCAR-FROM-AREA                          ELGACP  
01157 *    (ELUCONDB USES THE TEXT COMPRESSION WORK AREA, WHICH CAUSES  ELGACP  
01158 *    THE LOSS OF ANYTHING IN TCAR-FROM-AREA.  WE MUST SAVE THE    ELGACP  
01159 *    CURRENT CONTENT OF TCAR-FROM-AREA AND RESTORE IT AFTER       ELGACP  
01160 *    OBTAINING THE TRANSLATION OF THE CONDITION BITS.)            ELGACP  
01161      MOVE TCAR-FROM-SUB TO WS-TEXT-HOLD-COUNT.                    ELGACP  
01162      MOVE TCAR-FROM-AREA TO WS-TEXT-HOLD-TEXT.                    ELGACP  
01163                                                                   ELGACP  
01164 * -- GET CONDITION BIT TRANSLATION                                ELGACP  
01165      MOVE ACCUM-CONDITION  TO  CMF-CONDITION-BITS.                ELGACP  
01166      CALL 'ELUCONDB' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACP  
01167                                                                   ELGACP  
01168      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGACP  
01169      CALL 'ELUSETAD'                                              ELGACP  
01170         USING DFHCOMMAREA                                         ELGACP  
01171               ADDRESS OF CMF-DESCR.                               ELGACP  
01172                                                                   ELGACP  
01173 * -- RESTORE THE CONTENTS OF TCAR-FROM-AREA                       ELGACP  
01174      MOVE WS-TEXT-HOLD-COUNT TO TCAR-FROM-SUB.                    ELGACP  
01175      MOVE WS-TEXT-HOLD-TEXT TO TCAR-FROM-AREA.                    ELGACP  
01176                                                                   ELGACP  
01177 * -- APPEND THE TRANSLATION                                       ELGACP  
01178      PERFORM 0550-MOVE-TRANSLATION-TO-COMPR.                      ELGACP  
01179                                                                   ELGACP  
01180 /***********************************************************      ELGACP  
01181 *                                                          *      ELGACP  
01182 *    GENERATE PLACE OF TREATMENT PHRASE                    *      ELGACP  
01183 *                                                          *      ELGACP  
01184 ************************************************************      ELGACP  
01185  0410-GEN-PLC-OF-TRTMT-PH.                                        ELGACP  
01186      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01187      MOVE PH-WD-PROVIDED TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGACP  
01188                                                                   ELGACP  
01189      MOVE ACCUM-PLACE-OF-TREATMENT  TO  CMF-CODE-VALUE.           ELGACP  
01190      MOVE 'COPAY-PLACE-OF-TREATMENT' TO CMF-ELEMENT-SYSTEM-NAME.  ELGACP  
01191      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01192                                                                   ELGACP  
01193 ************************************************************      ELGACP  
01194 *                                                          *      ELGACP  
01195 *    GENERATE PATIENT RELATIONSHIP PHRASE                  *      ELGACP  
01196 *                                                          *      ELGACP  
01197 ************************************************************      ELGACP  
01198  0420-GEN-PT-RLTNSHP-PH.                                          ELGACP  
01199      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01200      MOVE PH-WD-TO TO TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGACP  
01201                                                                   ELGACP  
01202      MOVE ACCUM-RELATIONSHIP-IND   TO  CMF-CODE-VALUE.            ELGACP  
01203      MOVE 'COPAY-RELATIONSHIP-IND' TO  CMF-ELEMENT-SYSTEM-NAME.   ELGACP  
01204      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01205                                                                   ELGACP  
01206 /***********************************************************      ELGACP  
01207 *                                                          *      ELGACP  
01208 *    GENERATE PATIENT RELATIONSHIP AND AGE PHRASE          *      ELGACP  
01209 *                                                          *      ELGACP  
01210 ************************************************************      ELGACP  
01211  0430-GEN-PT-RLTNSHP-AGE-PH.                                      ELGACP  
01212      IF RELATIONSHIP-IND-NA                                       ELGACP  
01213      THEN                                                         ELGACP  
01214         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
01215         MOVE PH-NO-PT-RLTNSHP TO TCAR-FROM-LINE (TCAR-FROM-SUB)   ELGACP  
01216      ELSE                                                         ELGACP  
01217         PERFORM 0420-GEN-PT-RLTNSHP-PH.                           ELGACP  
01218                                                                   ELGACP  
01219      PERFORM 0440-GEN-FROM-AGE-PH.                                ELGACP  
01220                                                                   ELGACP  
01221      PERFORM 0450-GEN-TO-AGE-PH.                                  ELGACP  
01222                                                                   ELGACP  
01223 /***********************************************************      ELGACP  
01224 *                                                          *      ELGACP  
01225 *    GENERATE FROM AGE PHRASE                              *      ELGACP  
01226 *                                                          *      ELGACP  
01227 ************************************************************      ELGACP  
01228  0440-GEN-FROM-AGE-PH.                                            ELGACP  
01229      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01230      MOVE PH-PT-FROM-AGE TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGACP  
01231                                                                   ELGACP  
01232      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01233      IF ACCUM-AGE-LIMIT-FROM-UNLIM                                ELGACP  
01234      THEN                                                         ELGACP  
01235         MOVE PH-WD-UNLIMITED TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELGACP  
01236      ELSE                                                         ELGACP  
01237         MOVE ACCUM-AGE-LIMIT-FROM-VAL TO PH-AGE-LIM-FROM          ELGACP  
01238         MOVE PH-AGE-LIM-FROM TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGACP  
01239                                                                   ELGACP  
01240      MOVE ACCUM-AGE-LIMIT-FROM-IND  TO  CMF-CODE-VALUE.           ELGACP  
01241      MOVE 'COPAY-AGE-QUAL-IND-FROM' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGACP  
01242      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01243                                                                   ELGACP  
01244 ************************************************************      ELGACP  
01245 *                                                          *      ELGACP  
01246 *    GENERATE TO AGE PHRASE                                *      ELGACP  
01247 *                                                          *      ELGACP  
01248 ************************************************************      ELGACP  
01249  0450-GEN-TO-AGE-PH.                                              ELGACP  
01250      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01251      MOVE PH-PT-TO-AGE TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELGACP  
01252                                                                   ELGACP  
01253      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01254      IF ACCUM-AGE-LIMIT-TO-UNLIM                                  ELGACP  
01255      THEN                                                         ELGACP  
01256         MOVE PH-WD-UNLIMITED TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELGACP  
01257      ELSE                                                         ELGACP  
01258         MOVE ACCUM-AGE-LIMIT-TO-VAL TO PH-AGE-LIM-TO              ELGACP  
01259         MOVE PH-AGE-LIM-TO TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGACP  
01260                                                                   ELGACP  
01261      MOVE ACCUM-AGE-LIMIT-TO-IND    TO  CMF-CODE-VALUE.           ELGACP  
01262      MOVE 'COPAY-AGE-QUAL-IND-TO'   TO  CMF-ELEMENT-SYSTEM-NAME.  ELGACP  
01263      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01264                                                                   ELGACP  
01265 /***********************************************************      ELGACP  
01266 *                                                          *      ELGACP  
01267 *    GENERATE INTERNAL TABULARS LIST                       *      ELGACP  
01268 *                                                          *      ELGACP  
01269 ************************************************************      ELGACP  
01270  0460-GEN-INTRNL-TAB-LST.                                         ELGACP  
01271      INITIALIZE WS-INT-TAB-LST                                    ELGACP  
01272                 WS-INT-TAB-TXT                                    ELGACP  
01273                 WS-TAB-SUB.                                       ELGACP  
01274                                                                   ELGACP  
01275      IF SW-HAS-IBGR                                               ELGACP  
01276         ADD 1 TO WS-TAB-SUB                                       ELGACP  
01277         SET WS-INT-TAB-IBGR (WS-TAB-SUB)                          ELGACP  
01278             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACP  
01279                                                                   ELGACP  
01280      IF SW-HAS-IDGD                                               ELGACP  
01281         ADD 1 TO WS-TAB-SUB                                       ELGACP  
01282         SET WS-INT-TAB-IDGD (WS-TAB-SUB)                          ELGACP  
01283             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACP  
01284                                                                   ELGACP  
01285      IF SW-HAS-IPGN                                               ELGACP  
01286         ADD 1 TO WS-TAB-SUB                                       ELGACP  
01287         SET WS-INT-TAB-IPGN (WS-TAB-SUB)                          ELGACP  
01288             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACP  
01289                                                                   ELGACP  
01290      IF SW-HAS-IPGP                                               ELGACP  
01291         ADD 1 TO WS-TAB-SUB                                       ELGACP  
01292         SET WS-INT-TAB-IPGP (WS-TAB-SUB)                          ELGACP  
01293             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACP  
01294                                                                   ELGACP  
01295      IF SW-HAS-IPGT                                               ELGACP  
01296         ADD 1 TO WS-TAB-SUB                                       ELGACP  
01297         SET WS-INT-TAB-IPGT (WS-TAB-SUB)                          ELGACP  
01298             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACP  
01299                                                                   ELGACP  
01300      IF SW-HAS-IPGS                                               ELGACP  
01301         ADD 1 TO WS-TAB-SUB                                       ELGACP  
01302         SET WS-INT-TAB-IPGS (WS-TAB-SUB)                          ELGACP  
01303             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGACP  
01304                                                                   ELGACP  
01305      IF WS-TAB-SUB > 0                                            ELGACP  
01306         SET WS-INT-TAB-END (WS-TAB-SUB) TO TRUE                   ELGACP  
01307         IF  WS-TAB-SUB > 1                                        ELGACP  
01308         THEN                                                      ELGACP  
01309            SET WS-INT-TAB-AND (WS-TAB-SUB - 1) TO TRUE            ELGACP  
01310         END-IF                                                    ELGACP  
01311         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
01312         MOVE PH-INTRNL-TAB-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB) ELGACP  
01313         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
01314         MOVE WS-INT-TAB-TXT-1 TO TCAR-FROM-LINE (TCAR-FROM-SUB)   ELGACP  
01315         IF WS-TAB-SUB > 3                                         ELGACP  
01316         THEN                                                      ELGACP  
01317            ADD 1 TO TCAR-FROM-SUB                                 ELGACP  
01318            MOVE WS-INT-TAB-TXT-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB)ELGACP  
01319         END-IF                                                    ELGACP  
01320         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
01321         MOVE PH-INTRNL-TAB-TRAIL TO TCAR-FROM-LINE (TCAR-FROM-SUB)ELGACP  
01322      END-IF.                                                      ELGACP  
01323                                                                   ELGACP  
01324 /***********************************************************      ELGACP  
01325 *                                                          *      ELGACP  
01326 *    GENERATE DEFINITION SENTENCE                          *      ELGACP  
01327 *                                                          *      ELGACP  
01328 ************************************************************      ELGACP  
01329  0510-GEN-DEFN-SENT.                                              ELGACP  
01330      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01331      MOVE PH-DEFN-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELGACP  
01332                                                                   ELGACP  
01333      MOVE ACCUM-DEFINITION      TO CMF-CODE-VALUE.                ELGACP  
01334      MOVE 'COPAY-DEFINITION'    TO CMF-ELEMENT-SYSTEM-NAME.       ELGACP  
01335      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01336                                                                   ELGACP  
01337      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01338      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGACP  
01339                                                                   ELGACP  
01340      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
01341                                                                   ELGACP  
01342 /***********************************************************      ELGACP  
01343 *                                                          *      ELGACP  
01344 *    GENERATE TOPIC COPAY OCCURRENCE TRAILER               *      ELGACP  
01345 *                                                          *      ELGACP  
01346 ************************************************************      ELGACP  
01347  0520-GEN-TOPIC-TRAILER.                                          ELGACP  
01348      IF TCAR-FROM-SUB > 0                                         ELGACP  
01349      THEN                                                         ELGACP  
01350         PERFORM 0610-COMPLETE-AND-SEND-PARA.                      ELGACP  
01351      ADD 1 TO TCAR-FROM-SUB                                       ELGACP  
01352      MOVE PH-SEE-BP-TOPICS-1 TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGACP  
01353      ADD 1 TO TCAR-FROM-SUB                                       ELGACP  
01354      MOVE PH-SEE-BP-TOPICS-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGACP  
01355      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
01356                                                                   ELGACP  
01357 ************************************************************      ELGACP  
01358 *                                                          *      ELGACP  
01359 *    GENERATE BENEFIT PROVISION DED OCCURRENCE TRAILER     *      ELGACP  
01360 *                                                          *      ELGACP  
01361 ************************************************************      ELGACP  
01362  0530-GEN-BP-TRAILER.                                             ELGACP  
01363      IF TCAR-FROM-SUB > 0                                         ELGACP  
01364      THEN                                                         ELGACP  
01365         PERFORM 0610-COMPLETE-AND-SEND-PARA.                      ELGACP  
01366      ADD 1 TO TCAR-FROM-SUB                                       ELGACP  
01367      MOVE PH-SEE-ACP-TOPIC TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGACP  
01368      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
01369                                                                   ELGACP  
01370 /***********************************************************      ELGACP  
01371 *                                                          *      ELGACP  
01372 *    TRANSLATE ACCUMULATOR CODE VALUE                      *      ELGACP  
01373 *    AND MOVE TO COMPRESS WORK AREA                        *      ELGACP  
01374 *                                                          *      ELGACP  
01375 ************************************************************      ELGACP  
01376  0540-XLAT-ACP-CODE-VAL.                                          ELGACP  
01377      MOVE '#ACP' TO CMF-RECORD-PREFIX.                            ELGACP  
01378      EXEC CICS LINK PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)      ELGACP  
01379         END-EXEC.                                                 ELGACP  
01380                                                                   ELGACP  
01381      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGACP  
01382      CALL 'ELUSETAD'                                              ELGACP  
01383         USING DFHCOMMAREA                                         ELGACP  
01384               ADDRESS OF CMF-DESCR.                               ELGACP  
01385                                                                   ELGACP  
01386      PERFORM 0550-MOVE-TRANSLATION-TO-COMPR.                      ELGACP  
01387                                                                   ELGACP  
01388 ************************************************************      ELGACP  
01389 *                                                          *      ELGACP  
01390 *        MOVE TRANSLATION TO COMPRESS AREA                 *      ELGACP  
01391 *                                                          *      ELGACP  
01392 ************************************************************      ELGACP  
01393  0550-MOVE-TRANSLATION-TO-COMPR.                                  ELGACP  
01394      PERFORM WITH TEST BEFORE                                     ELGACP  
01395            VARYING CMF-DESCR-IDX FROM 1 BY 1                      ELGACP  
01396              UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES            ELGACP  
01397         IF TCAR-FROM-SUB >= 20                                    ELGACP  
01398         THEN                                                      ELGACP  
01399            PERFORM 0600-SEND-PART-PARA                            ELGACP  
01400         END-IF                                                    ELGACP  
01401         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
01402         MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                       ELGACP  
01403           TO TCAR-FROM-LINE (TCAR-FROM-SUB)                       ELGACP  
01404         END-PERFORM.                                              ELGACP  
01405                                                                   ELGACP  
01406 /***********************************************************      ELGACP  
01407 *                                                          *      ELGACP  
01408 *    SEND A PARTIAL PARAGRAPH TO OUTPUT                    *      ELGACP  
01409 *                                                          *      ELGACP  
01410 ************************************************************      ELGACP  
01411  0600-SEND-PART-PARA.                                             ELGACP  
01412 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGACP  
01413      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGACP  
01414 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGACP  
01415      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGACP  
01416      MOVE +79                                                     ELGACP  
01417        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGACP  
01418           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGACP  
01419           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGACP  
01420           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGACP  
01421           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGACP  
01422           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGACP  
01423           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGACP  
01424           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGACP  
01425           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGACP  
01426           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGACP  
01427 * -- UNSTRING/FLOW THE OUTPUT                                     ELGACP  
01428      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGACP  
01429 * -- MOVE FORMATTED TEXT TO OUTPUT ** EXCEPT LAST LINE **         ELGACP  
01430      PERFORM WITH TEST BEFORE                                     ELGACP  
01431            VARYING TCAR-X FROM 1 BY 1                             ELGACP  
01432              UNTIL TCAR-X = TCAR-OUTPUT-FIELDS-USED               ELGACP  
01433 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGACP  
01434         IF COF-NBR-DTL-LINES >= 20                                ELGACP  
01435         THEN                                                      ELGACP  
01436            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGACP  
01437            INITIALIZE COF-DTL                                     ELGACP  
01438            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGACP  
01439         END-IF                                                    ELGACP  
01440 *    -- APPEND LINE TO OUTPUT                                     ELGACP  
01441         ADD 1 TO COF-NBR-DTL-LINES                                ELGACP  
01442         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGACP  
01443           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGACP  
01444         END-PERFORM.                                              ELGACP  
01445 * -- PUT LAST LINE OF COMPRESSED/UNSTRUNG OUTPUT INTO FROM AREA   ELGACP  
01446      INITIALIZE TCAR-FROM-AREA                                    ELGACP  
01447                 TCAR-FROM-LENGTH                                  ELGACP  
01448                 TCAR-FROM-SUB.                                    ELGACP  
01449      MOVE 1 TO TCAR-FROM-SUB.                                     ELGACP  
01450      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELGACP  
01451        TO TCAR-FROM-LINE (1).                                     ELGACP  
01452                                                                   ELGACP  
01453 /***********************************************************      ELGACP  
01454 *                                                          *      ELGACP  
01455 *    FINISH AA SENTENCE                                    *      ELGACP  
01456 *                                                          *      ELGACP  
01457 ************************************************************      ELGACP  
01458 *0605-FINISH-SENTENCE.                                            ELGACP  
01459 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01460 *    MOVE WS-ACCUMULATING-PHRASE TO TCAR-FROM-LINE (TCAR-FROM-SUB)ELGACP  
01461 *    MOVE ACCUM-COPAY-VALUE-LIMIT (WS-AA-INDEX) TO                ELGACP  
01462 *         PH-ACP-VAL-LIMIT-DOLLARS.                               ELGACP  
01463                                                                   ELGACP  
01464 /***********************************************************      ELGACP  
01465 *                                                          *      ELGACP  
01466 *    COMPLETE AND SEND A PARAGRAPH TO OUTPUT               *      ELGACP  
01467 *                                                          *      ELGACP  
01468 ************************************************************      ELGACP  
01469  0610-COMPLETE-AND-SEND-PARA.                                     ELGACP  
01470      PERFORM 9999-COMPRESS-UNSTRING.                              ELGACP  
01471 * -- MOVE FORMATTED TEXT TO OUTPUT                                ELGACP  
01472      PERFORM WITH TEST AFTER                                      ELGACP  
01473            VARYING TCAR-X FROM 1 BY 1                             ELGACP  
01474              UNTIL TCAR-X > TCAR-OUTPUT-FIELDS-USED               ELGACP  
01475 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGACP  
01476         IF COF-NBR-DTL-LINES >= 20                                ELGACP  
01477         THEN                                                      ELGACP  
01478            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGACP  
01479            INITIALIZE COF-DTL                                     ELGACP  
01480            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGACP  
01481         END-IF                                                    ELGACP  
01482 *    -- APPEND LINE TO OUTPUT                                     ELGACP  
01483         ADD 1 TO COF-NBR-DTL-LINES                                ELGACP  
01484         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGACP  
01485           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGACP  
01486         END-PERFORM.                                              ELGACP  
01487                                                                   ELGACP  
01488 * -- CLEAR THE COMPRESSION WORK AREA                              ELGACP  
01489      INITIALIZE TCAR-FROM-AREA                                    ELGACP  
01490                 TCAR-FROM-SUB.                                    ELGACP  
01491 * -- INSERT A BLANK LINE                                          ELGACP  
01492 *    ADD 1 TO COF-NBR-DTL-LINES.                                  ELGACP  
01493 *    MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELGACP  
01494 * -- CALL THE OUTPUT MODULE                                       ELGACP  
01495      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA.                  ELGACP  
01496      INITIALIZE COF-DTL.                                          ELGACP  
01497      MOVE ZERO TO COF-NBR-DTL-LINES.                              ELGACP  
01498                                                                   ELGACP  
01499 /***********************************************************      ELGACP  
01500 *                                                          *      ELGACP  
01501 *    GENERATE INTERNAL TABULAR LISTINGS                    *      ELGACP  
01502 *                                                          *      ELGACP  
01503 ************************************************************      ELGACP  
01504  0650-GEN-INTRNL-TAB-LSTNGS.                                      ELGACP  
01505                                                                   ELGACP  
01506      IF SW-HAS-IBGR                                               ELGACP  
01507         MOVE PC-IBGR TO SRP-INTERNAL-TAB-ID                       ELGACP  
01508         EXEC CICS LINK PROGRAM ('ELGIBGR')                        ELGACP  
01509                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGACP  
01510                                                                   ELGACP  
01511      IF SW-HAS-IDGD                                               ELGACP  
01512         MOVE PC-IDGD TO SRP-INTERNAL-TAB-ID                       ELGACP  
01513         EXEC CICS LINK PROGRAM ('ELGIDGD')                        ELGACP  
01514                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGACP  
01515                                                                   ELGACP  
01516      IF SW-HAS-IPGN                                               ELGACP  
01517         MOVE PC-IPGN TO SRP-INTERNAL-TAB-ID                       ELGACP  
01518         EXEC CICS LINK PROGRAM ('ELGIPGN')                        ELGACP  
01519                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGACP  
01520                                                                   ELGACP  
01521      IF SW-HAS-IPGP                                               ELGACP  
01522         MOVE PC-IPGP TO SRP-INTERNAL-TAB-ID                       ELGACP  
01523         EXEC CICS LINK PROGRAM ('ELGIPGP')                        ELGACP  
01524                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGACP  
01525                                                                   ELGACP  
01526      IF SW-HAS-IPGT                                               ELGACP  
01527         MOVE PC-IPGT TO SRP-INTERNAL-TAB-ID                       ELGACP  
01528         EXEC CICS LINK PROGRAM ('ELGIPGT')                        ELGACP  
01529                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGACP  
01530                                                                   ELGACP  
01531      IF SW-HAS-IPGS                                               ELGACP  
01532         MOVE PC-IPGS TO SRP-INTERNAL-TAB-ID                       ELGACP  
01533         EXEC CICS LINK PROGRAM ('ELGIPGS')                        ELGACP  
01534                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGACP  
01535                                                                   ELGACP  
01536 /***********************************************************      ELGACP  
01537 *                                                          *      ELGACP  
01538 *    READ ACCUMULATOR WORK FILE RECORD                     *      ELGACP  
01539 *                                                          *      ELGACP  
01540 ************************************************************      ELGACP  
01541                                                                   ELGACP  
01542  0660-READ-ACCUM-WORK-FILE-REC.                                   ELGACP  
01543      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGACP  
01544      CALL 'ELUSETAD'                                              ELGACP  
01545          USING DFHCOMMAREA                                        ELGACP  
01546                ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.            ELGACP  
01547      SET IOP-FCQ-NONE         TO  TRUE.                           ELGACP  
01548      SET IOP-KVQ-NONE         TO  TRUE.                           ELGACP  
01549      SET IOP-STG-MODE-LOCATE  TO  TRUE.                           ELGACP  
01550      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACP  
01551      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR.       ELGACP  
01552                                                                   ELGACP  
01553 ************************************************************      ELGACP  
01554 *                                                          *      ELGACP  
01555 *    DELETE ACCUMULATOR WORK FILE                          *      ELGACP  
01556 *                                                          *      ELGACP  
01557 ************************************************************      ELGACP  
01558                                                                   ELGACP  
01559  0670-DELETE-ACCUM-WORK-FILE.                                     ELGACP  
01560      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGACP  
01561      CALL 'ELUSETAD'                                              ELGACP  
01562         USING DFHCOMMAREA                                         ELGACP  
01563               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELGACP  
01564      SET  IOP-DEL           TO  TRUE.                             ELGACP  
01565      SET  IOP-FCQ-NONE      TO  TRUE.                             ELGACP  
01566      SET  IOP-KVQ-NONE      TO  TRUE.                             ELGACP  
01567      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACP  
01568                                                                   ELGACP  
01569 /***********************************************************      ELGACP  
01570 *                                                          *      ELGACP  
01571 *        END THE DISPLAY                                   *      ELGACP  
01572 *                                                          *      ELGACP  
01573 ************************************************************      ELGACP  
01574  0680-END-THE-DISPLAY.                                            ELGACP  
01575      IF COF-NBR-DTL-LINES > 0                                     ELGACP  
01576         CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.      ELGACP  
01577      SET COF-END  TO  TRUE.                                       ELGACP  
01578      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELGACP  
01579      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGACP  
01580      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGACP  
01581 /***********************************************************      ELGACP  
01582 *THIS WILL NOT INCLUDE 0A 0B 0F                            *      ELGACP  
01583 *        COMPLEX COMPLEX DISPLAY                           *      ELGACP  
01584 * THIS IS FIRST SENTENCE FOR EACH COPAY OCCUR              *      ELGACP  
01585 ************************************************************      ELGACP  
01586  0600-COMPLEX-COPAY.                                              ELGACP  
01587      PERFORM 0605-DO-CHAINED-OCCURS                               ELGACP  
01588        WITH TEST BEFORE                                           ELGACP  
01589        UNTIL WS-COUNTER-ACP = ACCUM-COPAY-COUNT.                  ELGACP  
01590                                                                   ELGACP  
01591 /***********************************************************      ELGACP  
01592 *THIS IS FOR 0A 0B 0F                                      *      ELGACP  
01593 *        PAIRED DISPLAY                                    *      ELGACP  
01594 * THIS IS FIRST SENTENCE FOR EACH COPAY OCCUR              *      ELGACP  
01595 ************************************************************      ELGACP  
01596  0600-PAIRED-DISPLAY.                                             ELGACP  
01597      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01598      MOVE PH-UP-TO TO TCAR-FROM-LINE(TCAR-FROM-SUB).              ELGACP  
01599      ADD 1 TO TCAR-FROM-SUB                                       ELGACP  
01600      MOVE ACCUM-COPAY-VALUE-LIMIT (COPAY-INDEX)                   ELGACP  
01601         TO  PH-ACP-VAL-LMT-DOLLARS                                ELGACP  
01602      MOVE PH-ACP-VAL-LMT-DOLLARS TO                               ELGACP  
01603         TCAR-FROM-LINE (TCAR-FROM-SUB)                            ELGACP  
01604      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01605      PERFORM 0601-SELECT-BEN-PER-TRANS.                           ELGACP  
01606      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01607 *    SET COPAY-INDEX UP BY 3.                                     ELGACP  
01608      ADD 1 TO TCAR-FROM-SUB                                       ELGACP  
01609      MOVE WS-PERIOD TO TCAR-FROM-LINE(TCAR-FROM-SUB)              ELGACP  
01610      PERFORM 0920-COMPLETE-AND-SEND-PARA.                         ELGACP  
01611                                                                   ELGACP  
01612                                                                   ELGACP  
01613 /***********************************************************      ELGACP  
01614 * 0601-SELECT-BEN-PER-TRANS                                *      ELGACP  
01615 * THIS PARA IS PROVIDING A SPECIFIC ENGLISH TRNASLATION    *      ELGACP  
01616 * FOR ACP BENEFIT PERIODS AS THE CODES MANUAL DEFINITIONS  *      ELGACP  
01617 * ARE NOT ADEQUATE FOR THE INQUIRY FOLKS.                  *      ELGACP  
01618 *                                                          *      ELGACP  
01619 ************************************************************      ELGACP  
01620  0601-SELECT-BEN-PER-TRANS.                                       ELGACP  
01621      EVALUATE TRUE                                                ELGACP  
01622      WHEN ACCUM-COPAY-BEN-PER (COPAY-INDEX) = 'CA'                ELGACP  
01623          MOVE WS-PER-DATE TO TCAR-FROM-LINE(TCAR-FROM-SUB)        ELGACP  
01624      WHEN ACCUM-COPAY-BEN-PER (COPAY-INDEX) = 'CB'                ELGACP  
01625          MOVE WS-PER-CLAIM TO TCAR-FROM-LINE(TCAR-FROM-SUB)       ELGACP  
01626      WHEN ACCUM-COPAY-BEN-PER (COPAY-INDEX) = 'CC'                ELGACP  
01627          MOVE WS-PER-CALENDER-YEAR TO                             ELGACP  
01628                   TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELGACP  
01629      WHEN ACCUM-COPAY-BEN-PER (COPAY-INDEX) = 'CD'                ELGACP  
01630          MOVE WS-PER-CONTRACT-YEAR TO                             ELGACP  
01631                   TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELGACP  
01632      WHEN ACCUM-COPAY-BEN-PER (COPAY-INDEX) = 'CE'                ELGACP  
01633          MOVE WS-PER-LIFETIME TO                                  ELGACP  
01634                   TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELGACP  
01635      WHEN OTHER                                                   ELGACP  
01636          MOVE 'CODE VALUE NOT DEFINED' TO                         ELGACP  
01637                   TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELGACP  
01638      END-EVALUATE.                                                ELGACP  
01639                                                                   ELGACP  
01640 /***********************************************************      ELGACP  
01641 * 0605 DOING CHAINED OCCURS                                *      ELGACP  
01642 * DISPLAY NEEDS TO BE $S FOLLOWED BY TIME.. EVEN THOUGH    *      ELGACP  
01643 * ON GCPS THE TIME IS FIRST.. THIS MEANS THAT THE INDEX    *      ELGACP  
01644 * AND SUBSCRIPT MUST BE MANIPULATED BECAUSE TIME AND DOLLAR*      ELGACP  
01645 *ARE A PAIR OF OCCURS. OCCUR #2 IS TRANSLATE FOLLOWED BY #1*      ELGACP  
01646 *#4 FOLLOWED BY #3 ETC.  THIS APLIES TO CHAINED OCCURS     *      ELGACP  
01647 ************************************************************      ELGACP  
01648  0605-DO-CHAINED-OCCURS.                                          ELGACP  
01649      IF COPAY-INDEX > 2                                           ELGACP  
01650        MOVE PC-CHANGES-TO TO WS-MVL-CHANGES-PHRASE                ELGACP  
01651      ELSE                                                         ELGACP  
01652        MOVE SPACES TO WS-MVL-CHANGES-PHRASE                       ELGACP  
01653      END-IF.                                                      ELGACP  
01654      ADD 1 TO WS-ACP-SUB.                                         ELGACP  
01655      MOVE 1 TO TCAR-FROM-SUB.                                     ELGACP  
01656      PERFORM 0652-DETERMINE-VALUE-QUALIFIER.                      ELGACP  
01657      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01658      MOVE PH-APPLIES TO TCAR-FROM-LINE(TCAR-FROM-SUB).            ELGACP  
01659      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01660      MOVE ACCUM-COPAY-BEN-PER (COPAY-INDEX)                       ELGACP  
01661               TO  CMF-CODE-VALUE.                                 ELGACP  
01662      MOVE 'COPAY-BENEFIT-PERIOD' TO  CMF-ELEMENT-SYSTEM-NAME.     ELGACP  
01663      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01664      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01665      MOVE PH-UP-TO TO TCAR-FROM-LINE(TCAR-FROM-SUB).              ELGACP  
01666      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01667      SET COPAY-INDEX DOWN BY 1.                                   ELGACP  
01668      COMPUTE WS-ACP-SUB = WS-ACP-SUB - 1.                         ELGACP  
01669      PERFORM 0652-DETERMINE-VALUE-QUALIFIER.                      ELGACP  
01670 *    PERFORM 0920-COMPLETE-AND-SEND-PARA.                         ELGACP  
01671      SET COPAY-INDEX UP BY 3.                                     ELGACP  
01672      ADD 2 TO WS-ACP-SUB.                                         ELGACP  
01673      ADD 2 TO WS-COUNTER-ACP.                                     ELGACP  
01674      IF WS-COUNTER-ACP = ACCUM-COPAY-COUNT                        ELGACP  
01675         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
01676         MOVE WS-PERIOD TO TCAR-FROM-LINE(TCAR-FROM-SUB)           ELGACP  
01677      END-IF.                                                      ELGACP  
01678      PERFORM 0920-COMPLETE-AND-SEND-PARA.                         ELGACP  
01679 *     PERFORM 0605-DISPLAY-REMAINING-OCCURS.                      ELGACP  
01680                                                                   ELGACP  
01681 /***********************************************************      ELGACP  
01682 *                                                          *      ELGACP  
01683 *       DISPLAY LEAD IN PHRASE                             *      ELGACP  
01684 *                                                          *      ELGACP  
01685 ************************************************************      ELGACP  
01686  0625-DISPLAY-LEADIN-PHRASE.                                      ELGACP  
01687      MOVE 1 TO TCAR-FROM-SUB.                                     ELGACP  
01688      MOVE PH-APPLIC-1-LEAD-C TO                                   ELGACP  
01689            TCAR-FROM-LINE(TCAR-FROM-SUB).                         ELGACP  
01690      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01691      MOVE ACCUM-FAM-OR-INDIV  TO  CMF-CODE-VALUE.                 ELGACP  
01692      MOVE 'COPAY-FAM-OR-INDIV' TO  CMF-ELEMENT-SYSTEM-NAME.       ELGACP  
01693      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01694 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01695 *    MOVE PH-WD-FOR TO TCAR-FROM-LINE(TCAR-FROM-SUB).             ELGACP  
01696 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01697 *     MOVE ACCUM-L-O-B  TO  CMF-CODE-VALUE.                       ELGACP  
01698 *     MOVE 'COPAY-L-O-B' TO  CMF-ELEMENT-SYSTEM-NAME.             ELGACP  
01699 *     PERFORM 0540-XLAT-ACP-CODE-VAL.                             ELGACP  
01700      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01701      MOVE WS-AS-FOLLOWS TO TCAR-FROM-LINE(TCAR-FROM-SUB).         ELGACP  
01702      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01703      MOVE WS-COLON TO TCAR-FROM-LINE(TCAR-FROM-SUB).              ELGACP  
01704 *    PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
01705                                                                   ELGACP  
01706 /***********************************************************      ELGACP  
01707 *                                                          *      ELGACP  
01708 *       DISPLAY-REMAINING OCCURS                           *      ELGACP  
01709 *                                                          *      ELGACP  
01710 ************************************************************      ELGACP  
01711 *0605-DISPLAY-REMAINING-OCCURS.                                   ELGACP  
01712 *    PERFORM VARYING COPAY-INDEX FROM 1 BY 1 UNTIL                ELGACP  
01713 *       COPAY-INDEX > ACCUM-COPAY-COUNT                           ELGACP  
01714 *       IF ACCUM-COPAY-DEFINITION (COPAY-INDEX) = '0A'            ELGACP  
01715 *         OR '0B' OR '0C'                                         ELGACP  
01716 *         PERFORM 0615-DISPLAY-DEFINITION-TYPES                   ELGACP  
01717 *         END-IF                                                  ELGACP  
01718 *         IF ACCUM-COPAY-TIME-DOL-IND (COPAY-INDEX) =             ELGACP  
01719 *          'A1' OR 'B1' OR 'C1' OR 'D1' OR 'E1'                   ELGACP  
01720 *          PERFORM 0630-DISPLAY-CHAINING-TYPES                    ELGACP  
01721 *             UNTIL COPAY-INDEX > ACCUM-COPAY-COUNT               ELGACP  
01722 *        END-IF                                                   ELGACP  
01723 *      END-PERFORM.                                               ELGACP  
01724 /***********************************************************      ELGACP  
01725 *                                                          *      ELGACP  
01726 *       DISPLAY DEFINTION TYPES                            *      ELGACP  
01727 *                                                          *      ELGACP  
01728 ************************************************************      ELGACP  
01729 *0615-DISPLAY-DEFINITION-TYPES.                                   ELGACP  
01730 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01731 *    PERFORM 0652-DETERMINE-VALUE-QUALIFIER.                      ELGACP  
01732 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01733 *    MOVE PH-UP-TO TO TCAR-FROM-LINE(TCAR-FROM-SUB).              ELGACP  
01734 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01735 *    SET COPAY-INDEX UP BY 1.                                     ELGACP  
01736 *    PERFORM 0652-DETERMINE-VALUE-QUALIFIER.                      ELGACP  
01737 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01738 *    MOVE ACCUM-COPAY-BEN-PER(COPAY-INDEX) TO                     ELGACP  
01739 *             CMF-CODE-VALUE.                                     ELGACP  
01740 *    MOVE 'COPAY-BENEFIT-PERIOD' TO  CMF-ELEMENT-SYSTEM-NAME.     ELGACP  
01741 *    PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01742 *    PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
01743 /***********************************************************      ELGACP  
01744 *                                                          *      ELGACP  
01745 *       DISPLAY CHAINING TYPES                             *      ELGACP  
01746 *                                                          *      ELGACP  
01747 ************************************************************      ELGACP  
01748 *0630-DISPLAY-CHAINING-TYPES.                                     ELGACP  
01749 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01750 *    PERFORM 0652-DETERMINE-VALUE-QUALIFIER.                      ELGACP  
01751 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01752 *    MOVE PH-WD-AT TO TCAR-FROM-LINE(TCAR-FROM-SUB).              ELGACP  
01753 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01754 *    SET COPAY-INDEX UP BY 1.                                     ELGACP  
01755 *    ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01756 *    MOVE PH-CHANGING-TO TO TCAR-FROM-LINE(TCAR-FROM-SUB).        ELGACP  
01757 *    PERFORM 0652-DETERMINE-VALUE-QUALIFIER.                      ELGACP  
01758 *    PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGACP  
01759                                                                   ELGACP  
01760 /***********************************************************      ELGACP  
01761 *                                                          *      ELGACP  
01762 *        SIMPLE COPAY DISPLAY                              *      ELGACP  
01763 * THIS IS FIRST SENTENCE FOR EACH COPAY OCCUR              *      ELGACP  
01764 ************************************************************      ELGACP  
01765  0650-SIMPLE-COPAY.                                               ELGACP  
01766      MOVE 1 TO TCAR-FROM-SUB.                                     ELGACP  
01767      MOVE PH-APPLIC-1-LEAD TO TCAR-FROM-LINE(TCAR-FROM-SUB).      ELGACP  
01768      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01769      PERFORM 0652-DETERMINE-VALUE-QUALIFIER.                      ELGACP  
01770      ADD 1 TO TCAR-FROM-SUB                                       ELGACP  
01771      MOVE PH-APPLIES TO TCAR-FROM-LINE(TCAR-FROM-SUB).            ELGACP  
01772      ADD 1 TO TCAR-FROM-SUB                                       ELGACP  
01773      PERFORM 0601-SELECT-BEN-PER-TRANS                            ELGACP  
01774      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01775      MOVE WS-PER     TO TCAR-FROM-LINE(TCAR-FROM-SUB).            ELGACP  
01776      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01777      MOVE ACCUM-FAM-OR-INDIV  TO  CMF-CODE-VALUE.                 ELGACP  
01778      MOVE 'COPAY-FAM-OR-INDIV' TO  CMF-ELEMENT-SYSTEM-NAME.       ELGACP  
01779      PERFORM 0540-XLAT-ACP-CODE-VAL.                              ELGACP  
01780      ADD 1 TO TCAR-FROM-SUB.                                      ELGACP  
01781       ADD 1 TO TCAR-FROM-SUB.                                     ELGACP  
01782       MOVE WS-PERIOD TO TCAR-FROM-LINE(TCAR-FROM-SUB).            ELGACP  
01783       PERFORM 0610-COMPLETE-AND-SEND-PARA.                        ELGACP  
01784                                                                   ELGACP  
01785 /***********************************************************      ELGACP  
01786 *                                                          *      ELGACP  
01787 *        DETERMINE VALUE QUALIFIER                         *      ELGACP  
01788 *                                                          *      ELGACP  
01789 ************************************************************      ELGACP  
01790  0652-DETERMINE-VALUE-QUALIFIER.                                  ELGACP  
01791      IF ACCUM-COPAY-VALUE-QUALIFIER (COPAY-INDEX)        = '5'    ELGACP  
01792        MOVE ACCUM-COPAY-VALUE-LIMIT (COPAY-INDEX)                 ELGACP  
01793            TO PH-ACP-VAL-LMT-DOLLARS                              ELGACP  
01794        MOVE PH-ACP-VAL-LMT-DOLLARS                                ELGACP  
01795          TO TCAR-FROM-LINE (TCAR-FROM-SUB)                        ELGACP  
01796      ELSE                                                         ELGACP  
01797         MOVE ACCUM-COPAY-VAL-LIM-NO-DOL (COPAY-INDEX)             ELGACP  
01798                    TO PH-ACP-VAL-LMT-OTHER                        ELGACP  
01799         ADD 1 TO TCAR-FROM-SUB                                    ELGACP  
01800         MOVE PH-ACP-VAL-LMT-OTHER TO                              ELGACP  
01801                    TCAR-FROM-LINE (TCAR-FROM-SUB)                 ELGACP  
01802         MOVE ACCUM-COPAY-VALUE-QUALIFIER (COPAY-INDEX)            ELGACP  
01803                     TO CMF-CODE-VALUE                             ELGACP  
01804         MOVE 'COPAY-VALUE-QUALIFIER' TO CMF-ELEMENT-SYSTEM-NAME   ELGACP  
01805         PERFORM 0540-XLAT-ACP-CODE-VAL                            ELGACP  
01806      END-IF.                                                      ELGACP  
01807                                                                   ELGACP  
01808 /***********************************************************      ELGACP  
01809 *                                                          *      ELGACP  
01810 *        COMPLETE AND SEND PARAGRAPH                       *      ELGACP  
01811 * THIS IS USED TO TAKE OUT EXTRA CREATED IS 0610 PARA      *      ELGACP  
01812 * THE COMPLEX PARAGRAPHS ARE SINGLE SPACED ON OUTPUT       *      ELGACP  
01813 ************************************************************      ELGACP  
01814  0910-COMPLETE-AND-SEND-PARA.                                     ELGACP  
01815      PERFORM 9999-COMPRESS-UNSTRING.                              ELGACP  
01816 * -- MOVE FORMATTED TEXT TO OUTPUT                                ELGACP  
01817      PERFORM                                                      ELGACP  
01818            VARYING TCAR-X FROM 1 BY 1                             ELGACP  
01819              UNTIL TCAR-X > TCAR-OUTPUT-FIELDS-USED               ELGACP  
01820 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGACP  
01821         IF COF-NBR-DTL-LINES >= 20                                ELGACP  
01822         THEN                                                      ELGACP  
01823            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGACP  
01824            INITIALIZE COF-DTL                                     ELGACP  
01825            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGACP  
01826         END-IF                                                    ELGACP  
01827 *    -- APPEND LINE TO OUTPUT                                     ELGACP  
01828         ADD 1 TO COF-NBR-DTL-LINES                                ELGACP  
01829         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGACP  
01830           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGACP  
01831         END-PERFORM.                                              ELGACP  
01832                                                                   ELGACP  
01833 * -- CLEAR THE COMPRESSION WORK AREA                              ELGACP  
01834      INITIALIZE TCAR-FROM-AREA                                    ELGACP  
01835                 TCAR-FROM-SUB.                                    ELGACP  
01836 * -- INSERT A BLANK LINE                                          ELGACP  
01837 *    ADD 1 TO COF-NBR-DTL-LINES.                                  ELGACP  
01838 *    MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELGACP  
01839 * -- CALL THE OUTPUT MODULE                                       ELGACP  
01840      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA.                  ELGACP  
01841      INITIALIZE COF-DTL.                                          ELGACP  
01842      MOVE ZERO TO COF-NBR-DTL-LINES.                              ELGACP  
01843                                                                   ELGACP  
01844 /***********************************************************      ELGACP  
01845 *                                                          *      ELGACP  
01846 *        COMPLETE AND SEND PARAGRAPH                       *      ELGACP  
01847 * THIS IS USED FOR CHAINED OUT PUT REQUIRUNG CUSTOM  LINES *      ELGACP  
01848 *                                                          *      ELGACP  
01849 ************************************************************      ELGACP  
01850  0920-COMPLETE-AND-SEND-PARA.                                     ELGACP  
01851      PERFORM 9999-COMPRESS-UNSTRING.                              ELGACP  
01852      IF TCAR-OUTPUT-FIELDS-USED = 1                               ELGACP  
01853         MOVE 1 TO COF-NBR-DTL-LINES                               ELGACP  
01854         MOVE 1 TO TCAR-X                                          ELGACP  
01855         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGACP  
01856            TO WS-MVL-MASK                                         ELGACP  
01857         MOVE WS-MULTI-VALUE-LINE TO COF-DTL-LINE(1)               ELGACP  
01858         CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL       ELGACP  
01859         INITIALIZE COF-DTL                                        ELGACP  
01860         MOVE ZERO TO COF-NBR-DTL-LINES                            ELGACP  
01861      ELSE                                                         ELGACP  
01862 * -- MOVE FORMATTED TEXT TO OUTPUT                                ELGACP  
01863        PERFORM                                                    ELGACP  
01864              VARYING TCAR-X FROM 1 BY 1                           ELGACP  
01865                UNTIL TCAR-X > TCAR-OUTPUT-FIELDS-USED             ELGACP  
01866 *      -- SEND THE OUTPUT BUFFER IF FULL                          ELGACP  
01867           IF COF-NBR-DTL-LINES >= 20                              ELGACP  
01868           THEN                                                    ELGACP  
01869              CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL  ELGACP  
01870              INITIALIZE COF-DTL                                   ELGACP  
01871              MOVE ZERO TO COF-NBR-DTL-LINES                       ELGACP  
01872           END-IF                                                  ELGACP  
01873 *      -- APPEND LINE TO OUTPUT                                   ELGACP  
01874           ADD 1 TO COF-NBR-DTL-LINES                              ELGACP  
01875           MOVE TCAR-OPF-DATA (TCAR-X)                             ELGACP  
01876             TO COF-DTL-LINE (COF-NBR-DTL-LINES)                   ELGACP  
01877           END-PERFORM                                             ELGACP  
01878        END-IF.                                                    ELGACP  
01879                                                                   ELGACP  
01880 * -- CLEAR THE COMPRESSION WORK AREA                              ELGACP  
01881      INITIALIZE TCAR-FROM-AREA                                    ELGACP  
01882                 TCAR-FROM-SUB.                                    ELGACP  
01883 * -- INSERT A BLANK LINE                                          ELGACP  
01884 *    ADD 1 TO COF-NBR-DTL-LINES.                                  ELGACP  
01885 *    MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELGACP  
01886 * -- CALL THE OUTPUT MODULE                                       ELGACP  
01887      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA.                  ELGACP  
01888      INITIALIZE COF-DTL.                                          ELGACP  
01889      MOVE ZERO TO COF-NBR-DTL-LINES.                              ELGACP  
01890 *                                                                 ELGACP  
01891  9999-COMPRESS-UNSTRING.                                          ELGACP  
01892 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGACP  
01893      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGACP  
01894 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGACP  
01895      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGACP  
01896      MOVE +79                                                     ELGACP  
01897        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGACP  
01898           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGACP  
01899           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGACP  
01900           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGACP  
01901           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGACP  
01902           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGACP  
01903           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGACP  
01904           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGACP  
01905           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGACP  
01906           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGACP  
01907 * -- UNSTRING/FLOW THE OUTPUT                                     ELGACP  
01908      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGACP  
