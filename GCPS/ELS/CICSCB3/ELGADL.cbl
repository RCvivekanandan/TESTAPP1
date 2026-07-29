00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELGADL  
00003  PROGRAM-ID.           ELGADL.                                       LV002
00004                                                                   ELGADL  
00005  AUTHOR.               GEORGE E. MOORE.                           ELGADL  
00006                                                                   ELGADL  
00007  INSTALLATION.         HEALTH CARE SERVICE CORPORATION            ELGADL  
00008                        A MUTUAL LEGAL RESERVE COMPANY             ELGADL  
00009                        BLUE CROSS/BLUE SHIELD OF ILLINOIS         ELGADL  
00010                        233 N. MICHIGAN AVE                        ELGADL  
00011                        CHICAGO, ILLINOIS 60601                    ELGADL  
00012                                                                   ELGADL  
00013  DATE-WRITTEN.         10-JUN-1992.                               ELGADL  
00014                                                                   ELGADL  
00015  DATE-COMPILED.                                                   ELGADL  
00016                                                                   ELGADL  
00017  SECURITY.             COPYRIGHT 1986, 1991                       ELGADL  
00018                        HEALTH CARE SERVICE CORPORATION            ELGADL  
00019      SKIP3                                                        ELGADL  
00020  ENVIRONMENT DIVISION.                                            ELGADL  
00021                                                                   ELGADL  
00022  CONFIGURATION SECTION.                                           ELGADL  
00023  SOURCE-COMPUTER.      IBM-3090.                                  ELGADL  
00024  OBJECT-COMPUTER.      IBM-3090.                                  ELGADL  
00025 /*****************************************************************ELGADL  
00026 *                                                                *ELGADL  
00027 *  ELGADL   - ELS:  GENERATES THE OUTPUT FOR DEDUCTIBLES AT      *ELGADL  
00028 *                   COST CONTAINMENT, TOPIC, AND THE BENEFIT     *ELGADL  
00029 *                   PROVISION LEVEL.                             *ELGADL  
00030 *                                                                *ELGADL  
00031 ******************************************************************ELGADL  
00032 *                                                                *ELGADL  
00033 *                      MAINTENANCE HISTORY                       *ELGADL  
00034 *                                                                *ELGADL  
00035 *  MOD     DATE     BY  DRPT                ACTION               *ELGADL  
00036 * ----- ----------- --- ----- ---------------------------------- *ELGADL  
00037 * 01.00 10-JUN-1992 GEM       CLONED FROM ELGABM.                *ELGADL  
00038 *                                                                *ELGADL  
00039 * 01.01 12-OCT-1992 JPB       CORRECTED MISSPELLING OF DED-BASE- *ELGADL  
00040 *                             AMT-SOURCE-IND                     *ELGADL  
00041 *                                                                *ELGADL  
00042 * 01.02 17-DEC-1992 JPB       CHANGED DEDUCTIBLE VALUE SENTENCE  *ELGADL  
00043 *                             LEAD PHRASE                        *ELGADL  
00044 *                                                                *ELGADL  
00045 * 01.03 26-MAR-1993 JPB       MOVED PERFORM END-THE-DISPLAY TO   *ELGADL  
00046 *                             PROCESS ROUTINE TO STOP ABEND WHEN *ELGADL  
00047 *                             THERE IS NO ACCUM TO PROCESS.      *ELGADL  
00048 *                                                                *ELGADL  
00049 * 01.04 06-APR-1993 JPB       ADDED LOGIC TO DISPLAY CARRY-OVER- *ELGADL  
00050 *                             CREDIT-INDICATOR.                  *ELGADL  
00051 *                                                                 ELGADL  
00052 * 01.05 24-AUG-2000 AKK       ADDED SUPPORT FOR #IPGS.            ELGADL  
00053 *                                                                *ELGADL  
00054 * 01.07 08-JAN-2003 AKK       REGEN'D FOR ADDN OF SMI AND NSM     ELGADL  
00055 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGADL  
ED0624*                                                                *        
ED0624* BBDA-58217 06/14/24  ED     RECOMPILE FOR PEAQ COPYBOOK        *        
ED0624*                             EXPANSION:                         *        
ED0624*                                 COPYBOOK ELSACUMC              *        
ED0624*                                                                *        
00056 ******************************************************************ELGADL  
00057      TITLE 'WORKING STORAGE SECTION'.                             ELGADL  
00058  DATA DIVISION.                                                   ELGADL  
00059  WORKING-STORAGE SECTION.                                         ELGADL  
00060                                                                   ELGADL  
00061 * -- HEADERS                                                      ELGADL  
00062                                                                   ELGADL  
00063   01  HD-TOPIC-HDR .                                              ELGADL  
00064       02                          PICTURE  X(34) VALUE SPACES.    ELGADL  
00065       02                          PICTURE  X(11) VALUE            ELGADL  
00066          'DEDUCTIBLES'.                                           ELGADL  
00067       02                          PICTURE  X(34) VALUE SPACES.    ELGADL  
00068                                                                   ELGADL  
00069   01  HD-BEN-PROVN-HDR.                                           ELGADL  
00070       02                          PICTURE  X(28) VALUE            ELGADL  
00071          'DEDUCTIBLE AT BENEFIT LEVEL:'.                          ELGADL  
00072       02                          PICTURE  X(51) VALUE SPACES.    ELGADL  
00073                                                                   ELGADL  
00074 * -- PHRASES AND TERMS - CONSTANT                                 ELGADL  
00075                                                                   ELGADL  
00076  01  PH-APPLIC-1-LEAD            PICTURE  X(27) VALUE             ELGADL  
00077      'THIS DEDUCTIBLE APPLIES PER'.                               ELGADL  
00078                                                                   ELGADL  
00079  01  PH-APPLIC-2-LINK            PICTURE  X(21) VALUE             ELGADL  
00080      'DEDUCTIBLE APPLIES TO'.                                     ELGADL  
00081                                                                   ELGADL  
00082  01  PH-CARRY-OVER               PICTURE  X(37) VALUE             ELGADL  
00083      'THE DEDUCTIBLE CARRY OVER CREDIT IS: '.                     ELGADL  
00084                                                                   ELGADL  
00085  01  PH-DEFN-LEAD                PICTURE  X(32) VALUE             ELGADL  
00086      'DOLLARS ARE ACCUMULATED BASED ON'.                          ELGADL  
00087                                                                   ELGADL  
00088  01  PH-INTRNL-TAB-LEAD          PICTURE  X(12) VALUE             ELGADL  
00089      ', SUBJECT TO'.                                              ELGADL  
00090                                                                   ELGADL  
00091  01  PH-INTRNL-TAB-TRAIL         PICTURE  X(27) VALUE             ELGADL  
00092      'CONSIDERATIONS LISTED BELOW'.                               ELGADL  
00093                                                                   ELGADL  
00094  01  PH-INTRVL-OVRD-LEAD         PICTURE  X(06) VALUE             ELGADL  
00095      '(NOTE:'.                                                    ELGADL  
00096                                                                   ELGADL  
00097  01  PH-INTRVL-OVRD-TRAIL        PICTURE  X(02) VALUE             ELGADL  
00098      ').'.                                                        ELGADL  
00099                                                                   ELGADL  
00100  01  PH-INTRVL-OVRD-SPCL-1-LEAD  PICTURE  X(32) VALUE             ELGADL  
00101      'THE INTERVAL MAY BE OVERRULED IF'.                          ELGADL  
00102                                                                   ELGADL  
00103  01  PH-INTRVL-OVRD-SPCL-1-TRAIL PICTURE  X(75) VALUE             ELGADL  
00104      'MONTHS HAVE ELAPSED FROM THE ADMISSION DATE OF THE FIRST COVELGADL  
00105 -    'ERED ADMISSION.'.                                           ELGADL  
00106                                                                   ELGADL  
00107  01  PH-INTRVL-OVRD-SPCL-2-LEAD  PICTURE  X(72) VALUE             ELGADL  
00108      'IF THE MEMBER IS MEDICARE ELIGIBLE, THE BENEFIT PERIODS ARE ELGADL  
00109 -    'SEPARATED BY'.                                              ELGADL  
00110                                                                   ELGADL  
00111  01  PH-INTRVL-OVRD-SPCL-2-TRAIL PICTURE  X(05) VALUE             ELGADL  
00112      'DAYS.'.                                                     ELGADL  
00113                                                                   ELGADL  
00114  01  PH-INTRVL-OVRD-SPCL-3-LEAD  PICTURE  X(37) VALUE             ELGADL  
00115      'THE INTERVAL CAN BE OVERRULED SO THAT'.                     ELGADL  
00116                                                                   ELGADL  
00117  01  PH-INTRVL-OVRD-SPCL-3-TRAIL PICTURE  X(52) VALUE             ELGADL  
00118      'DAYS/VISITS ARE PAID AT THE INDICATED PERCENT LEVEL.'.      ELGADL  
00119                                                                   ELGADL  
00120  01  PH-DED-VAL-SENT-LEAD         PICTURE  X(22) VALUE            ELGADL  
00121      'THERE IS A DEDUCTIBLE '.                                    ELGADL  
00122                                                                   ELGADL  
00123  01  PH-DED-OTHR-SRCE-LEAD        PICTURE  X(09) VALUE            ELGADL  
00124      'FOUND IN '.                                                 ELGADL  
00125                                                                   ELGADL  
00126  01  PH-DED-OTHR-SRCE-UNKN        PICTURE  X(15) VALUE            ELGADL  
00127      'ANOTHER SOURCE'.                                            ELGADL  
00128                                                                   ELGADL  
00129  01  PH-NO-DED                   PICTURE  X(45) VALUE             ELGADL  
00130      'NO GROUP OR CONTRACT LEVEL DEDUCTIBLES APPLY.'.             ELGADL  
00131                                                                   ELGADL  
00132  01  PH-NO-INST-DED.                                              ELGADL  
00133      02 FILLER                   PICTURE  X(35) VALUE             ELGADL  
00134           'NO INSTITUTIONAL GROUP OR CONTRACT '.                  ELGADL  
00135      02 FILLER                   PICTURE  X(24) VALUE             ELGADL  
00136           'LEVEL DEDUCTIBLES APPLY.'.                             ELGADL  
00137                                                                   ELGADL  
00138  01  PH-NO-PROF-DED.                                              ELGADL  
00139      02 FILLER                   PICTURE  X(34) VALUE             ELGADL  
00140           'NO PROFESSIONAL GROUP OR CONTRACT '.                   ELGADL  
00141      02 FILLER                   PICTURE  X(23) VALUE             ELGADL  
00142           'LEVEL DEDUCTIBLE APPLY.'.                              ELGADL  
00143                                                                   ELGADL  
00144  01  PH-NO-PT-RLTNSHP            PICTURE  X(11) VALUE             ELGADL  
00145      'TO PATIENTS'.                                               ELGADL  
00146                                                                   ELGADL  
00147  01  PH-PT-FROM-AGE              PICTURE  X(08) VALUE             ELGADL  
00148      'FROM AGE'.                                                  ELGADL  
00149                                                                   ELGADL  
00150  01  PH-PT-TO-AGE                PICTURE  X(06) VALUE             ELGADL  
00151      'TO AGE'.                                                    ELGADL  
00152                                                                   ELGADL  
00153  01  PH-REINST-LEAD              PICTURE  X(08) VALUE             ELGADL  
00154      ', AND IS'.                                                  ELGADL  
00155                                                                   ELGADL  
00156  01  PH-TIME-FCTR-LEAD           PICTURE  X(09) VALUE             ELGADL  
00157      'PERIOD OF'.                                                 ELGADL  
00158                                                                   ELGADL  
00159  01  PH-TIME-INTRVL-LEAD         PICTURE  X(12) VALUE             ELGADL  
00160      'SEPARATED BY'.                                              ELGADL  
00161                                                                   ELGADL  
00162  01  PH-SEE-BP-TOPICS-1.                                          ELGADL  
00163      02 FILLER                   PICTURE  X(27) VALUE             ELGADL  
00164      'ADDITIONAL DEDUCTIBLES MAY '.                               ELGADL  
00165      02 FILLER                   PICTURE  X(29) VALUE             ELGADL  
00166      'APPLY TO INDIVIDUAL BENEFITS.'.                             ELGADL  
00167                                                                   ELGADL  
00168  01  PH-SEE-BP-TOPICS-2          PICTURE  X(47) VALUE             ELGADL  
00169      'SEE SPECIFIC TOPICS FOR ADDITIONAL DEDUCTIBLES.'.           ELGADL  
00170                                                                   ELGADL  
00171  01  PH-SEE-DED-TOPIC             PICTURE  X(53) VALUE            ELGADL  
00172      'SEE THE DEDUCTIBLES TOPIC FOR ADDITIONAL DEDUCTIBLES.'.     ELGADL  
00173                                                                   ELGADL  
00174 * -- PHRASES AND TERMS - SINGLE WORDS                             ELGADL  
00175                                                                   ELGADL  
00176  01  PH-WD-BENEFITS    PICTURE  X(10) VALUE 'BENEFITS '.          ELGADL  
00177  01  PH-WD-FOR         PICTURE  X(04) VALUE 'FOR '.               ELGADL  
00178  01  PH-WD-OF          PICTURE  X(03) VALUE 'OF '.                ELGADL  
00179  01  PH-WD-PER         PICTURE  X(04) VALUE 'PER '.               ELGADL  
00180  01  PH-WD-PATIENTS    PICTURE  X(09) VALUE 'PATIENTS '.          ELGADL  
00181  01  PH-WD-PROVIDED    PICTURE  X(09) VALUE 'PROVIDED '.          ELGADL  
00182  01  PH-WD-SERVICES    PICTURE  X(09) VALUE 'SERVICES '.          ELGADL  
00183  01  PH-WD-THE         PICTURE  X(04) VALUE 'THE '.               ELGADL  
00184  01  PH-WD-THIS        PICTURE  X(05) VALUE 'THIS '.              ELGADL  
00185  01  PH-WD-TO          PICTURE  X(03) VALUE 'TO '.                ELGADL  
00186  01  PH-WD-UNLIMITED   PICTURE  X(10) VALUE 'UNLIMITED '.         ELGADL  
00187                                                                   ELGADL  
00188 * -- PHRASES AND TERMS - NUMERIC FORMAT AREAS                     ELGADL  
00189                                                                   ELGADL  
00190  01  PH-AGE-LIM-FROM             PICTURE  ZZ9B.                   ELGADL  
00191                                                                   ELGADL  
00192  01  PH-AGE-LIM-TO               PICTURE  ZZ9B.                   ELGADL  
00193                                                                   ELGADL  
00194  01  PH-BEN-PER-TIME-FCTR        PICTURE  ZZ9B.                   ELGADL  
00195                                                                   ELGADL  
00196  01  PH-INTRVL-TIME-FCTR         PICTURE  ZZ9B.                   ELGADL  
00197                                                                   ELGADL  
00198  01  PH-INTRVL-OVRD-VAL          PICTURE  ZZZZ9B.                 ELGADL  
00199                                                                   ELGADL  
00200  01  PH-DED-VAL-LMT-DOLLARS      PICTURE  $$,$$$,$$9.99B.         ELGADL  
00201                                                                   ELGADL  
00202  01  PH-DED-VAL-LMT-OTHER        PICTURE  ZZZ,ZZZ,Z99B.           ELGADL  
00203                                                                   ELGADL  
00204 * -- OTHERS                                                       ELGADL  
00205                                                                   ELGADL  
00206  01  PROGRAM-CONSTANTS.                                           ELGADL  
00207      05  PC-ADL                      PIC  X(06) VALUE             ELGADL  
00208              '#ADL  '.                                            ELGADL  
00209      05  PC-IBGR                     PIC  X(06) VALUE             ELGADL  
00210              '#IBGR '.                                            ELGADL  
00211      05  PC-IDGD                     PIC  X(06) VALUE             ELGADL  
00212              '#IDGD '.                                            ELGADL  
00213      05  PC-IPGN                     PIC  X(06) VALUE             ELGADL  
00214              '#IPGN '.                                            ELGADL  
00215      05  PC-IPGP                     PIC  X(06) VALUE             ELGADL  
00216              '#IPGP '.                                            ELGADL  
00217      05  PC-IPGT                     PIC  X(06) VALUE             ELGADL  
00218              '#IPGT '.                                            ELGADL  
00219      05  PC-IPGS                     PIC  X(06) VALUE             ELGADL  
00220              '#IPGS '.                                            ELGADL  
00221                                                                   ELGADL  
00222 / -- INTERNAL TABULAR INFORMATION WORK AREAS                      ELGADL  
00223 * -- INTERNAL TABULAR SWITCHES                                    ELGADL  
00224                                                                   ELGADL  
00225  01  WS-INT-TAB-SW.                                               ELGADL  
00226      02                          PICTURE  X(01).                  ELGADL  
00227         88 SW-HAS-INTERNALS      VALUE 'Y'.                       ELGADL  
00228         88 SW-HAS-NO-INTERNALS   VALUE 'N'.                       ELGADL  
00229      02                          PICTURE  X(01).                  ELGADL  
00230         88 SW-HAS-IBGR           VALUE 'Y'.                       ELGADL  
00231         88 SW-HAS-NO-IBGR        VALUE 'N'.                       ELGADL  
00232      02                          PICTURE  X(01).                  ELGADL  
00233         88 SW-HAS-IDGD           VALUE 'Y'.                       ELGADL  
00234         88 SW-HAS-NO-IDGD        VALUE 'N'.                       ELGADL  
00235      02                          PICTURE  X(01).                  ELGADL  
00236         88 SW-HAS-IPGN           VALUE 'Y'.                       ELGADL  
00237         88 SW-HAS-NO-IPGN        VALUE 'N'.                       ELGADL  
00238      02                          PICTURE  X(01).                  ELGADL  
00239         88 SW-HAS-IPGP           VALUE 'Y'.                       ELGADL  
00240         88 SW-HAS-NO-IPGP        VALUE 'N'.                       ELGADL  
00241      02                          PICTURE  X(01).                  ELGADL  
00242         88 SW-HAS-IPGT           VALUE 'Y'.                       ELGADL  
00243         88 SW-HAS-NO-IPGT        VALUE 'N'.                       ELGADL  
00244      02                          PICTURE  X(01).                  ELGADL  
00245         88 SW-HAS-IPGS           VALUE 'Y'.                       ELGADL  
00246         88 SW-HAS-NO-IPGS        VALUE 'N'.                       ELGADL  
00247                                                                   ELGADL  
00248  01  WS-INT-TAB-LST.                                              ELGADL  
00249      02 WS-TAB-SUB                PIC S9(04) COMP.                ELGADL  
00250      02 WS-INT-TAB                OCCURS 6 TIMES.                 ELGADL  
00251         03                        PIC  X(18).                     ELGADL  
00252            88 WS-INT-TAB-IBGR VALUE ' BENEFIT PROVISION'.         ELGADL  
00253            88 WS-INT-TAB-IDGD VALUE '         DIAGNOSIS'.         ELGADL  
00254            88 WS-INT-TAB-IPGN VALUE '   PROVIDER NUMBER'.         ELGADL  
00255            88 WS-INT-TAB-IPGP VALUE '         PROCEDURE'.         ELGADL  
00256            88 WS-INT-TAB-IPGT VALUE '     PROVIDER TYPE'.         ELGADL  
00257            88 WS-INT-TAB-IPGS VALUE 'PROVIDER SPECIALTY'.         ELGADL  
00258         03                        PIC  X(05).                     ELGADL  
00259            88 WS-INT-TAB-AND      VALUE ' AND '.                  ELGADL  
00260            88 WS-INT-TAB-COMMA    VALUE ',    '.                  ELGADL  
00261            88 WS-INT-TAB-END      VALUE SPACES.                   ELGADL  
00262                                                                   ELGADL  
00263  01  WS-INT-TAB-TXT               REDEFINES WS-INT-TAB-LST.       ELGADL  
00264      02                           PIC S9(04) COMP.                ELGADL  
00265      02 WS-INT-TAB-TXT-1          PICTURE  X(69).                 ELGADL  
00266      02 WS-INT-TAB-TXT-2          PICTURE  X(46).                 ELGADL  
00267                                                                   ELGADL  
00268  01  WS-TEXT-HOLD-AREA.                                           ELGADL  
00269      02 WS-TEXT-HOLD-COUNT       PICTURE S9(4)           COMP.    ELGADL  
00270      02 WS-TEXT-HOLD-TEXT        PICTURE  X(1580).                ELGADL  
00271      TITLE 'LINKAGE SECTION'.                                     ELGADL  
00272  LINKAGE SECTION.                                                 ELGADL  
00273                                                                   ELGADL  
00274  01  DFHCOMMAREA.                                                 ELGADL  
00275      COPY ELSCOMMC.                                               ELGADL  
00276 /                                                                 ELGADL  
00277      COPY ELSCIA2C.                                               ELGADL  
00278 /                                                                 ELGADL  
00279      COPY ELSIOPMC.                                               ELGADL  
00280 /                                                                 ELGADL  
00281      COPY ELSCMIFC.                                               ELGADL  
00282 /                                                                 ELGADL  
00283      COPY ELSCMDSC.                                               ELGADL  
00284 /                                                                 ELGADL  
00285      COPY ELSOUTPC.                                               ELGADL  
00286 /                                                                 ELGADL  
00287      COPY ELSSRTPC.                                               ELGADL  
00288 /                                                                 ELGADL  
00289      COPY ELSSSCBC.                                               ELGADL  
00290 /                                                                 ELGADL  
00291      COPY ELSTCWAC.                                               ELGADL  
00292 /                                                                 ELGADL  
00293      COPY ELSACUMC.                                               ELGADL  
00294 /                                                                 ELGADL  
00295  01  GCG-GROUP-SPECIFIC-RECORD.                                   ELGADL  
00296      COPY GCGROUPC.                                               ELGADL  
00297 /                                                                 ELGADL  
00298      TITLE 'PROCEDURE DIVISION'.                                  ELGADL  
00299 ************************************************************      ELGADL  
00300 *                                                          *      ELGADL  
00301 *    PROCEDURE DIVISION                                    *      ELGADL  
00302 *                                                          *      ELGADL  
00303 ************************************************************      ELGADL  
00304                                                                   ELGADL  
00305  PROCEDURE DIVISION.                                              ELGADL  
00306                                                                   ELGADL  
00307      PERFORM 0010-INITIALIZATION.                                 ELGADL  
00308      PERFORM 0130-PROCESS.                                        ELGADL  
00309      GOBACK.                                                      ELGADL  
00310                                                                   ELGADL  
00311 /***********************************************************      ELGADL  
00312 *                                                          *      ELGADL  
00313 *        INITIALIZATION                                    *      ELGADL  
00314 *                                                          *      ELGADL  
00315 ************************************************************      ELGADL  
00316                                                                   ELGADL  
00317  0010-INITIALIZATION.                                             ELGADL  
00318 * -- ESTABLISH STANDARD ENVIRONMENT                               ELGADL  
00319      PERFORM 0020-EST-ADR-OF-CONTROL-BLOCKS.                      ELGADL  
00320                                                                   ELGADL  
00321 * -- ESTABLISH ADDRESSABILITY OF WORK AREAS                       ELGADL  
00322      PERFORM 0070-EST-ADR-OF-TEMPORARY-FILE.                      ELGADL  
00323      PERFORM 0080-EST-ADR-OF-CDES-MANUAL.                         ELGADL  
00324      PERFORM 0090-EST-ADR-OF-OUTPUT-INTERFA.                      ELGADL  
00325      PERFORM 0100-EST-ADR-OF-SUBROUTINE-PAR.                      ELGADL  
00326      PERFORM 0110-EST-ADR-OF-TEXT-COMPRESSX.                      ELGADL  
00327      PERFORM 0120-EST-ADR-OF-GRP-SPC.                             ELGADL  
00328                                                                   ELGADL  
00329 * -- CLEAR OUTPUT AND INITIALIZE TEXT COMPRESSION WORK AREA       ELGADL  
00330      IF COF-NBR-DTL-LINES > 0                                     ELGADL  
00331      THEN                                                         ELGADL  
00332         CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.      ELGADL  
00333                                                                   ELGADL  
00334      INITIALIZE TCAR-FROM-AREA                                    ELGADL  
00335                 TCAR-FROM-LENGTH                                  ELGADL  
00336                 TCAR-FROM-SUB.                                    ELGADL  
00337                                                                   ELGADL  
00338 ************************************************************      ELGADL  
00339 *                                                          *      ELGADL  
00340 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGADL  
00341 *                                                          *      ELGADL  
00342 ************************************************************      ELGADL  
00343                                                                   ELGADL  
00344  0020-EST-ADR-OF-CONTROL-BLOCKS.                                  ELGADL  
00345      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGADL  
00346      THEN                                                         ELGADL  
00347         EXEC CICS ABEND ABCODE('EL01') END-EXEC                   ELGADL  
00348      ELSE                                                         ELGADL  
00349         IF ECA-CIA-PTR = NULL                                     ELGADL  
00350         THEN                                                      ELGADL  
00351            EXEC CICS ABEND ABCODE('EL02') END-EXEC                ELGADL  
00352         ELSE                                                      ELGADL  
00353            CALL 'ELUINISM'                                        ELGADL  
00354               USING DFHCOMMAREA                                   ELGADL  
00355                     ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA      ELGADL  
00356            SET CIA-ELSSSCB-DDN TO TRUE                            ELGADL  
00357            CALL 'ELUSETAD'                                        ELGADL  
00358               USING DFHCOMMAREA                                   ELGADL  
00359                     ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK        ELGADL  
00360            IF CIA-RC-PTR-NULL                                     ELGADL  
00361            THEN                                                   ELGADL  
00362               SET CIA-AB-UNALLOC-AREA TO TRUE                     ELGADL  
00363               EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC         ELGADL  
00364            ELSE                                                   ELGADL  
00365               CONTINUE                                            ELGADL  
00366            END-IF                                                 ELGADL  
00367         END-IF                                                    ELGADL  
00368      END-IF.                                                      ELGADL  
00369                                                                   ELGADL  
00370 /***********************************************************      ELGADL  
00371 *                                                          *      ELGADL  
00372 *        ESTABLISH ADDRESSABILITY OF TEMPORARY FILE        *      ELGADL  
00373 *                                                          *      ELGADL  
00374 ************************************************************      ELGADL  
00375                                                                   ELGADL  
00376  0070-EST-ADR-OF-TEMPORARY-FILE.                                  ELGADL  
00377      SET CIA-ELSWKFL1-DDN  TO TRUE.                               ELGADL  
00378      CALL 'ELUSETAD'                                              ELGADL  
00379         USING DFHCOMMAREA                                         ELGADL  
00380               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELGADL  
00381      IF CIA-RC-PTR-NULL                                           ELGADL  
00382         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADL  
00383         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGADL  
00384                                                                   ELGADL  
00385 ************************************************************      ELGADL  
00386 *                                                          *      ELGADL  
00387 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE*      ELGADL  
00388 *                                                          *      ELGADL  
00389 ************************************************************      ELGADL  
00390                                                                   ELGADL  
00391  0080-EST-ADR-OF-CDES-MANUAL.                                     ELGADL  
00392      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGADL  
00393      CALL 'ELUSETAD'                                              ELGADL  
00394         USING DFHCOMMAREA                                         ELGADL  
00395               ADDRESS OF CMF-CODES-MANUAL-INTERFACE.              ELGADL  
00396      IF CIA-RC-PTR-NULL                                           ELGADL  
00397         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADL  
00398         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGADL  
00399                                                                   ELGADL  
00400 ************************************************************      ELGADL  
00401 *                                                          *      ELGADL  
00402 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGADL  
00403 *                                                          *      ELGADL  
00404 ************************************************************      ELGADL  
00405                                                                   ELGADL  
00406  0090-EST-ADR-OF-OUTPUT-INTERFA.                                  ELGADL  
00407      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGADL  
00408      CALL 'ELUSETAD'                                              ELGADL  
00409         USING DFHCOMMAREA                                         ELGADL  
00410               ADDRESS OF COF-OUTPUT-INTERFACE.                    ELGADL  
00411      IF CIA-RC-PTR-NULL                                           ELGADL  
00412         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADL  
00413         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGADL  
00414                                                                   ELGADL  
00415 ************************************************************      ELGADL  
00416 *                                                          *      ELGADL  
00417 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGADL  
00418 *                                                          *      ELGADL  
00419 ************************************************************      ELGADL  
00420                                                                   ELGADL  
00421  0100-EST-ADR-OF-SUBROUTINE-PAR.                                  ELGADL  
00422      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGADL  
00423      CALL 'ELUSETAD'                                              ELGADL  
00424         USING DFHCOMMAREA                                         ELGADL  
00425               ADDRESS OF SRP-SUBROUTINE-PARAMETERS.               ELGADL  
00426      IF CIA-RC-PTR-NULL                                           ELGADL  
00427         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADL  
00428         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGADL  
00429                                                                   ELGADL  
00430 ************************************************************      ELGADL  
00431 *                                                          *      ELGADL  
00432 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELGADL  
00433 *                                                          *      ELGADL  
00434 ************************************************************      ELGADL  
00435                                                                   ELGADL  
00436  0110-EST-ADR-OF-TEXT-COMPRESSX.                                  ELGADL  
00437      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGADL  
00438      CALL 'ELUSETAD'                                              ELGADL  
00439         USING DFHCOMMAREA                                         ELGADL  
00440               ADDRESS OF TCAR-COMPRESSION-WORK-AREA.              ELGADL  
00441      IF CIA-RC-PTR-NULL                                           ELGADL  
00442         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADL  
00443         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGADL  
00444                                                                   ELGADL  
00445 ************************************************************      ELGADL  
00446 *                                                          *      ELGADL  
00447 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD *      ELGADL  
00448 *                                                          *      ELGADL  
00449 ************************************************************      ELGADL  
00450                                                                   ELGADL  
00451  0120-EST-ADR-OF-GRP-SPC.                                         ELGADL  
00452      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGADL  
00453      CALL 'ELUSETAD'                                              ELGADL  
00454         USING DFHCOMMAREA                                         ELGADL  
00455               ADDRESS OF GCG-GROUP-SPECIFIC-RECORD.               ELGADL  
00456      IF CIA-RC-PTR-NULL                                           ELGADL  
00457         SET CIA-AB-UNALLOC-AREA TO TRUE                           ELGADL  
00458         EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.              ELGADL  
00459                                                                   ELGADL  
00460 /***********************************************************      ELGADL  
00461 *                                                          *      ELGADL  
00462 *        PROCESS                                           *      ELGADL  
00463 *                                                          *      ELGADL  
00464 ************************************************************      ELGADL  
00465                                                                   ELGADL  
00466  0130-PROCESS.                                                    ELGADL  
00467 * -- GENERATE INITIAL HEADER                                      ELGADL  
00468      EVALUATE TRUE                                                ELGADL  
00469          WHEN SRP-TOPIC-ACCUM                                     ELGADL  
00470               PERFORM 0140-OBTAIN-TOPIC-HEADER                    ELGADL  
00471          WHEN SRP-BEN-PROV-ACCUM                                  ELGADL  
00472               PERFORM 0150-OBTAIN-BEN-PROVN-HEADER                ELGADL  
00473      END-EVALUATE.                                                ELGADL  
00474                                                                   ELGADL  
00475 * -- GENERATE ACCUMULATOR OUTPUT                                  ELGADL  
00476      EVALUATE TRUE                                                ELGADL  
00477         WHEN SRP-NO-ACCUMS-FOUND                                  ELGADL  
00478            PERFORM 0170-DSPLY-NO-ACCUMS                           ELGADL  
00479         WHEN SRP-INST-NOT-APPLICABLE                              ELGADL  
00480            PERFORM 0180-DSPLY-INST-NOT-APPLIC                     ELGADL  
00481         WHEN SRP-PROF-NOT-APPLICABLE                              ELGADL  
00482            PERFORM 0190-DSPLY-PROF-NOT-APPLIC                     ELGADL  
00483         WHEN OTHER                                                ELGADL  
00484            PERFORM 0200-DISPLAY-REGULAR-TEXT                      ELGADL  
00485      END-EVALUATE.                                                ELGADL  
00486      IF SRP-TOPIC-ACCUM                                           ELGADL  
00487         PERFORM 0680-END-THE-DISPLAY.                             ELGADL  
00488                                                                   ELGADL  
00489 /***********************************************************      ELGADL  
00490 *                                                          *      ELGADL  
00491 *        OBTAIN TOPIC HEADER                               *      ELGADL  
00492 *                                                          *      ELGADL  
00493 ************************************************************      ELGADL  
00494                                                                   ELGADL  
00495  0140-OBTAIN-TOPIC-HEADER.                                        ELGADL  
00496      MOVE +2            TO  COF-NBR-HDR-LINES.                    ELGADL  
00497      MOVE HD-TOPIC-HDR  TO  COF-HDR-LINE (2).                     ELGADL  
00498      MOVE  0            TO  COF-NBR-DTL-LINES.                    ELGADL  
00499      SET  COF-NEW-PAGE  TO  TRUE.                                 ELGADL  
00500      PERFORM 0160-SEND-INITL-HDR.                                 ELGADL  
00501                                                                   ELGADL  
00502 ************************************************************      ELGADL  
00503 *                                                          *      ELGADL  
00504 *        OBTAIN BENEFIT PROVISION HEADER                   *      ELGADL  
00505 *                                                          *      ELGADL  
00506 ************************************************************      ELGADL  
00507                                                                   ELGADL  
00508  0150-OBTAIN-BEN-PROVN-HEADER.                                    ELGADL  
00509      INITIALIZE COF-DTL-LINE (1).                                 ELGADL  
00510      MOVE HD-BEN-PROVN-HDR TO COF-DTL-LINE (2).                   ELGADL  
00511      SET  COF-CONTINUE  TO  TRUE.                                 ELGADL  
00512      MOVE +0            TO  COF-NBR-HDR-LINES.                    ELGADL  
00513      MOVE +2            TO  COF-NBR-DTL-LINES.                    ELGADL  
00514      PERFORM 0160-SEND-INITL-HDR.                                 ELGADL  
00515                                                                   ELGADL  
00516 ************************************************************      ELGADL  
00517 *                                                          *      ELGADL  
00518 *    SEND INITIAL HEADER                                   *      ELGADL  
00519 *                                                          *      ELGADL  
00520 ************************************************************      ELGADL  
00521                                                                   ELGADL  
00522  0160-SEND-INITL-HDR.                                             ELGADL  
00523      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGADL  
00524      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGADL  
00525      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADL  
00526      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGADL  
00527                                                                   ELGADL  
00528 /***********************************************************      ELGADL  
00529 *                                                          *      ELGADL  
00530 *        DISPLAY NO ACCUMS MESSAGE                         *      ELGADL  
00531 *                                                          *      ELGADL  
00532 ************************************************************      ELGADL  
00533                                                                   ELGADL  
00534  0170-DSPLY-NO-ACCUMS.                                            ELGADL  
00535      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00536      MOVE PH-NO-DED TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGADL  
00537      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGADL  
00538                                                                   ELGADL  
00539 ************************************************************      ELGADL  
00540 *                                                          *      ELGADL  
00541 *    DISPLAY INSTITUTIONAL DEDUCTIBLE NOT APPLICABLE       *      ELGADL  
00542 *                                                          *      ELGADL  
00543 ************************************************************      ELGADL  
00544                                                                   ELGADL  
00545  0180-DSPLY-INST-NOT-APPLIC.                                      ELGADL  
00546      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00547      MOVE PH-NO-INST-DED TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGADL  
00548      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGADL  
00549                                                                   ELGADL  
00550 ************************************************************      ELGADL  
00551 *                                                          *      ELGADL  
00552 *    DISPLAY PROFESSIONAL DEDUCTIBLES NOT APPLICABLE       *      ELGADL  
00553 *                                                          *      ELGADL  
00554 ************************************************************      ELGADL  
00555                                                                   ELGADL  
00556  0190-DSPLY-PROF-NOT-APPLIC.                                      ELGADL  
00557      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00558      MOVE PH-NO-PROF-DED TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGADL  
00559      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGADL  
00560                                                                   ELGADL  
00561 /***********************************************************      ELGADL  
00562 *                                                          *      ELGADL  
00563 *        DISPLAY REGULAR TEXT                              *      ELGADL  
00564 *                                                          *      ELGADL  
00565 ************************************************************      ELGADL  
00566                                                                   ELGADL  
00567  0200-DISPLAY-REGULAR-TEXT.                                       ELGADL  
00568 * -- DO INITIAL READ                                              ELGADL  
00569      MOVE 1 TO IOP-TSQ-ITEM-NBR.                                  ELGADL  
00570      SET  IOP-RD  TO  TRUE.                                       ELGADL  
00571      PERFORM 0660-READ-ACCUM-WORK-FILE-REC.                       ELGADL  
00572                                                                   ELGADL  
00573 * -- PROCESS UNTIL DONE                                           ELGADL  
00574      PERFORM 0210-DISPLAY-OCCURENCE-TEXT                          ELGADL  
00575          UNTIL NOT IOP-RC-OK.                                     ELGADL  
00576                                                                   ELGADL  
00577 * -- DELETE WORK FILE WHEN DONE                                   ELGADL  
00578      PERFORM 0670-DELETE-ACCUM-WORK-FILE.                         ELGADL  
00579                                                                   ELGADL  
00580 /***********************************************************      ELGADL  
00581 *                                                          *      ELGADL  
00582 *        DISPLAY OCCURENCE TEXT                            *      ELGADL  
00583 *                                                          *      ELGADL  
00584 ************************************************************      ELGADL  
00585                                                                   ELGADL  
00586  0210-DISPLAY-OCCURENCE-TEXT.                                     ELGADL  
00587                                                                   ELGADL  
00588      PERFORM 0220-INIT-OCCRNC-PROC.                               ELGADL  
00589                                                                   ELGADL  
00590      PERFORM 0230-DISPLAY-COMMON-BODY-TEXT.                       ELGADL  
00591                                                                   ELGADL  
00592      SET IOP-RD-NXT  TO  TRUE.                                    ELGADL  
00593      PERFORM 0660-READ-ACCUM-WORK-FILE-REC.                       ELGADL  
00594                                                                   ELGADL  
00595      EVALUATE TRUE                                                ELGADL  
00596          WHEN SRP-TOPIC-ACCUM                                     ELGADL  
00597               PERFORM 0520-GEN-TOPIC-TRAILER                      ELGADL  
00598               IF IOP-RC-OK                                        ELGADL  
00599                  THEN                                             ELGADL  
00600                     PERFORM 0140-OBTAIN-TOPIC-HEADER              ELGADL  
00601                  ELSE                                             ELGADL  
00602                     CONTINUE                                      ELGADL  
00603               END-IF                                              ELGADL  
00604          WHEN SRP-BEN-PROV-ACCUM                                  ELGADL  
00605               PERFORM 0530-GEN-BP-TRAILER                         ELGADL  
00606      END-EVALUATE.                                                ELGADL  
00607                                                                   ELGADL  
00608 ************************************************************      ELGADL  
00609 *                                                          *      ELGADL  
00610 *    INITIALIZE OCCURRENCE PROCESSING                      *      ELGADL  
00611 *                                                          *      ELGADL  
00612 ************************************************************      ELGADL  
00613  0220-INIT-OCCRNC-PROC.                                           ELGADL  
00614      SET SW-HAS-NO-INTERNALS                                      ELGADL  
00615          SW-HAS-NO-IBGR                                           ELGADL  
00616          SW-HAS-NO-IDGD                                           ELGADL  
00617          SW-HAS-NO-IPGN                                           ELGADL  
00618          SW-HAS-NO-IPGP                                           ELGADL  
00619          SW-HAS-NO-IPGT                                           ELGADL  
00620          SW-HAS-NO-IPGS                                           ELGADL  
00621       TO TRUE.                                                    ELGADL  
00622                                                                   ELGADL  
00623 /***********************************************************      ELGADL  
00624 *                                                          *      ELGADL  
00625 *        DISPLAY COMMON BODY TEXT                          *      ELGADL  
00626 *                                                          *      ELGADL  
00627 ************************************************************      ELGADL  
00628                                                                   ELGADL  
00629  0230-DISPLAY-COMMON-BODY-TEXT.                                   ELGADL  
00630                                                                   ELGADL  
00631      PERFORM 0240-CHK-INTRNL-TABS.                                ELGADL  
00632                                                                   ELGADL  
00633      PERFORM 0250-GEN-DED-VALUE-SENT.                             ELGADL  
00634                                                                   ELGADL  
00635      PERFORM 0350-GEN-APPLIC-SENT-1.                              ELGADL  
00636                                                                   ELGADL  
00637      PERFORM 0370-GEN-APPLIC-SENT-2.                              ELGADL  
00638                                                                   ELGADL  
00639      IF DEFINITION-NA                                             ELGADL  
00640      THEN                                                         ELGADL  
00641         CONTINUE                                                  ELGADL  
00642      ELSE                                                         ELGADL  
00643         PERFORM 0510-GEN-DEFN-SENT.                               ELGADL  
00644                                                                   ELGADL  
00645      SET ASC-DES-INDEX TO 1.                                      ELGADL  
00646      PERFORM 0650-GEN-INTRNL-TAB-LSTNGS.                          ELGADL  
00647                                                                   ELGADL  
00648 /***********************************************************      ELGADL  
00649 *                                                          *      ELGADL  
00650 *    CHECK FOR INTERNAL TABULARS                           *      ELGADL  
00651 *                                                          *      ELGADL  
00652 ************************************************************      ELGADL  
00653  0240-CHK-INTRNL-TABS.                                            ELGADL  
00654      IF NO-IBGR-SLOT-NBR (1)                                      ELGADL  
00655      THEN CONTINUE                                                ELGADL  
00656      ELSE SET SW-HAS-IBGR SW-HAS-INTERNALS TO TRUE.               ELGADL  
00657                                                                   ELGADL  
00658      IF NO-IDGD-SLOT-NBR (1)                                      ELGADL  
00659      THEN CONTINUE                                                ELGADL  
00660      ELSE SET SW-HAS-IDGD SW-HAS-INTERNALS TO TRUE.               ELGADL  
00661                                                                   ELGADL  
00662      IF NO-IPGN-SLOT-NBR (1)                                      ELGADL  
00663      THEN CONTINUE                                                ELGADL  
00664      ELSE SET SW-HAS-IPGN SW-HAS-INTERNALS TO TRUE.               ELGADL  
00665                                                                   ELGADL  
00666      IF NO-IPGP-SLOT-NBR (1)                                      ELGADL  
00667      THEN CONTINUE                                                ELGADL  
00668      ELSE SET SW-HAS-IPGP SW-HAS-INTERNALS TO TRUE.               ELGADL  
00669                                                                   ELGADL  
00670      IF NO-IPGT-SLOT-NBR (1)                                      ELGADL  
00671      THEN CONTINUE                                                ELGADL  
00672      ELSE SET SW-HAS-IPGT SW-HAS-INTERNALS TO TRUE.               ELGADL  
00673                                                                   ELGADL  
00674      IF NO-IPGS-SLOT-NBR (1)                                      ELGADL  
00675      THEN CONTINUE                                                ELGADL  
00676      ELSE SET SW-HAS-IPGS SW-HAS-INTERNALS TO TRUE.               ELGADL  
00677                                                                   ELGADL  
00678 /***********************************************************      ELGADL  
00679 *                                                          *      ELGADL  
00680 *        GENERATE DEDUCTIBLE DESCRIPTION                   *      ELGADL  
00681 *                                                          *      ELGADL  
00682 ************************************************************      ELGADL  
00683  0250-GEN-DED-VALUE-SENT.                                         ELGADL  
00684                                                                   ELGADL  
00685      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00686      MOVE PH-DED-VAL-SENT-LEAD TO TCAR-FROM-LINE(TCAR-FROM-SUB).  ELGADL  
00687                                                                   ELGADL  
00688      IF ACCUM-VALUE-LIMIT (1) < ZERO                              ELGADL  
00689      THEN                                                         ELGADL  
00690         PERFORM 0260-DISPLAY-DED-ELSEWHERE                        ELGADL  
00691      ELSE                                                         ELGADL  
00692         PERFORM 0270-GEN-STD-DED-PH                               ELGADL  
00693      END-IF.                                                      ELGADL  
00694                                                                   ELGADL  
00695      IF BENEFIT-PERIOD-NA                                         ELGADL  
00696      THEN                                                         ELGADL  
00697         CONTINUE                                                  ELGADL  
00698      ELSE                                                         ELGADL  
00699         PERFORM 0310-GEN-BEN-PER-PH.                              ELGADL  
00700                                                                   ELGADL  
00701      IF CARRY-OVER-CREDIT-IND-NA                                  ELGADL  
00702      THEN                                                         ELGADL  
00703         CONTINUE                                                  ELGADL  
00704      ELSE                                                         ELGADL  
00705         PERFORM 0315-GEN-CARRY-OVER-PHR.                          ELGADL  
00706                                                                   ELGADL  
00707      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGADL  
00708                                                                   ELGADL  
00709 /***********************************************************      ELGADL  
00710 *                                                          *      ELGADL  
00711 *        DISPLAY DEDUCTIBLE ELSEWHERE MESSAGE              *      ELGADL  
00712 *                                                          *      ELGADL  
00713 ************************************************************      ELGADL  
00714  0260-DISPLAY-DED-ELSEWHERE.                                      ELGADL  
00715      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00716      MOVE PH-DED-OTHR-SRCE-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).ELGADL  
00717                                                                   ELGADL  
00718      IF ACCUM-DED-BASE-AMT-SOURCE-IND NOT = ZERO                  ELGADL  
00719      THEN                                                         ELGADL  
00720         ADD 1 TO TCAR-FROM-SUB                                    ELGADL  
00721         MOVE PH-WD-THE TO TCAR-FROM-LINE (TCAR-FROM-SUB)          ELGADL  
00722         MOVE ACCUM-DED-BASE-AMT-SOURCE-IND TO CMF-CODE-VALUE      ELGADL  
00723         MOVE 'DED-BASE-AMT-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME ELGADL  
00724         MOVE 'GROUP' TO CMF-RECORD-PREFIX                         ELGADL  
00725         EXEC CICS LINK PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)   ELGADL  
00726            END-EXEC                                               ELGADL  
00727         SET CIA-ELSCMDSC-DDN TO TRUE                              ELGADL  
00728         CALL 'ELUSETAD' USING DFHCOMMAREA ADDRESS OF CMF-DESCR    ELGADL  
00729         PERFORM 0550-MOVE-TRANSLATION-TO-COMPR                    ELGADL  
00730      ELSE                                                         ELGADL  
00731         ADD 1 TO TCAR-FROM-SUB                                    ELGADL  
00732         MOVE PH-DED-OTHR-SRCE-UNKN                                ELGADL  
00733           TO TCAR-FROM-LINE (TCAR-FROM-SUB)                       ELGADL  
00734      END-IF.                                                      ELGADL  
00735                                                                   ELGADL  
00736      PERFORM 0600-SEND-PART-PARA.                                 ELGADL  
00737                                                                   ELGADL  
00738 /***********************************************************      ELGADL  
00739 *                                                          *      ELGADL  
00740 *    GENERATE STANDARD DEDUCTIBLE PHRASE                   *      ELGADL  
00741 *                                                          *      ELGADL  
00742 ************************************************************      ELGADL  
00743  0270-GEN-STD-DED-PH.                                             ELGADL  
00744      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00745      MOVE PH-WD-OF TO TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGADL  
00746                                                                   ELGADL  
00747      IF ACCUM-VAL-UNLIM (1)                                       ELGADL  
00748      THEN                                                         ELGADL  
00749         PERFORM 0280-GEN-UNLIM-TERM                               ELGADL  
00750      ELSE                                                         ELGADL  
00751         IF ACCUM-VALUE-QUALIFIER = '5'                            ELGADL  
00752         THEN                                                      ELGADL  
00753            PERFORM 0290-GEN-DOLLAR-LIM-TERM                       ELGADL  
00754         ELSE                                                      ELGADL  
00755            PERFORM 0300-GEN-OTHER-LIM-TERM                        ELGADL  
00756         END-IF                                                    ELGADL  
00757      END-IF.                                                      ELGADL  
00758      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00759      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGADL  
00760                                                                   ELGADL  
00761 ************************************************************      ELGADL  
00762 *                                                          *      ELGADL  
00763 *    GENERATE UNLIMITED DEDUCTIBLE TERM                    *      ELGADL  
00764 *                                                          *      ELGADL  
00765 ************************************************************      ELGADL  
00766  0280-GEN-UNLIM-TERM.                                             ELGADL  
00767      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00768      MOVE PH-WD-UNLIMITED TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELGADL  
00769      MOVE 'DEDL-VALUE-QUALIFIER' TO CMF-ELEMENT-SYSTEM-NAME.      ELGADL  
00770      MOVE ACCUM-VALUE-QUALIFIER TO CMF-CODE-VALUE                 ELGADL  
00771      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
00772                                                                   ELGADL  
00773 ************************************************************      ELGADL  
00774 *                                                          *      ELGADL  
00775 *    GENERATE DOLLAR DEDUCTIBLE TERM                       *      ELGADL  
00776 *                                                          *      ELGADL  
00777 ************************************************************      ELGADL  
00778  0290-GEN-DOLLAR-LIM-TERM.                                        ELGADL  
00779      MOVE ACCUM-VALUE-LIMIT (1) TO PH-DED-VAL-LMT-DOLLARS.        ELGADL  
00780      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00781      MOVE PH-DED-VAL-LMT-DOLLARS                                  ELGADL  
00782        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGADL  
00783                                                                   ELGADL  
00784 ************************************************************      ELGADL  
00785 *                                                          *      ELGADL  
00786 *    GENERATE OTHER DEDUCTIBLE TERM                        *      ELGADL  
00787 *                                                          *      ELGADL  
00788 ************************************************************      ELGADL  
00789  0300-GEN-OTHER-LIM-TERM.                                         ELGADL  
00790      MOVE ACCUM-VALUE-LIMIT-NON-DOLLAR (1) TO PH-DED-VAL-LMT-OTHERELGADL  
00791      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00792      MOVE PH-DED-VAL-LMT-OTHER TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGADL  
00793                                                                   ELGADL  
00794      MOVE ACCUM-VALUE-QUALIFIER TO CMF-CODE-VALUE                 ELGADL  
00795      MOVE 'DEDL-VALUE-QUALIFIER' TO CMF-ELEMENT-SYSTEM-NAME.      ELGADL  
00796      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
00797                                                                   ELGADL  
00798 /***********************************************************      ELGADL  
00799 *                                                          *      ELGADL  
00800 *    GENERATE BENEFIT PERIOD PHRASE                        *      ELGADL  
00801 *                                                          *      ELGADL  
00802 ************************************************************      ELGADL  
00803  0310-GEN-BEN-PER-PH.                                             ELGADL  
00804 * -- CLEAR COMPRESSION INPUT BUFFER                               ELGADL  
00805      IF TCAR-FROM-SUB > 1                                         ELGADL  
00806      THEN                                                         ELGADL  
00807         PERFORM 0600-SEND-PART-PARA.                              ELGADL  
00808                                                                   ELGADL  
00809      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00810      MOVE PH-WD-PER TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGADL  
00811                                                                   ELGADL  
00812      MOVE ACCUM-BENEFIT-PERIOD   TO  CMF-CODE-VALUE.              ELGADL  
00813      MOVE 'DEDL-BENEFIT-PERIOD'  TO  CMF-ELEMENT-SYSTEM-NAME.     ELGADL  
00814      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
00815                                                                   ELGADL  
00816                                                                   ELGADL  
00817      IF ACCUM-BEN-PER-TIME-QUAL NOT = ZEROS                       ELGADL  
00818      THEN                                                         ELGADL  
00819         PERFORM 0320-GEN-BEN-PER-TIME-FCTR-TRM.                   ELGADL  
00820                                                                   ELGADL  
00821      IF ACCUM-INTERVAL-TIME-FCTR NOT = ZEROS                      ELGADL  
00822      THEN                                                         ELGADL  
00823         PERFORM 0330-GEN-INTRVL-TIME-FCTR-TRM.                    ELGADL  
00824                                                                   ELGADL  
00825      IF INTERVAL-OVRD-IND-NA                                      ELGADL  
00826      THEN                                                         ELGADL  
00827         CONTINUE                                                  ELGADL  
00828      ELSE                                                         ELGADL  
00829         PERFORM 0340-GEN-INTRVL-OVRD-NOTE.                        ELGADL  
00830                                                                   ELGADL  
00831      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00832      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGADL  
00833                                                                   ELGADL  
00834 /***********************************************************      ELGADL  
00835 *                                                          *      ELGADL  
00836 *    GENERATE CARRY OVER CREDIT PHR                        *      ELGADL  
00837 *                                                          *      ELGADL  
00838 ************************************************************      ELGADL  
00839  0315-GEN-CARRY-OVER-PHR.                                         ELGADL  
00840      IF TCAR-FROM-SUB > 1                                         ELGADL  
00841         PERFORM 0600-SEND-PART-PARA.                              ELGADL  
00842      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00843      MOVE PH-CARRY-OVER TO TCAR-FROM-LINE (TCAR-FROM-SUB).        ELGADL  
00844                                                                   ELGADL  
00845      MOVE 'CARRY-OVER-CREDIT-IND'     TO CMF-ELEMENT-SYSTEM-NAME. ELGADL  
00846      MOVE ACCUM-CARRY-OVER-CREDIT-IND TO CMF-CODE-VALUE.          ELGADL  
00847      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
00848                                                                   ELGADL  
00849      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00850      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGADL  
00851                                                                   ELGADL  
00852 /***********************************************************      ELGADL  
00853 *                                                          *      ELGADL  
00854 *    GENERATE BENEFIT PERIOD TIME FACTOR TERM              *      ELGADL  
00855 *                                                          *      ELGADL  
00856 ************************************************************      ELGADL  
00857  0320-GEN-BEN-PER-TIME-FCTR-TRM.                                  ELGADL  
00858      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00859      MOVE PH-TIME-FCTR-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELGADL  
00860                                                                   ELGADL  
00861      MOVE ACCUM-BEN-PER-TIME-FCTR TO PH-BEN-PER-TIME-FCTR.        ELGADL  
00862      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00863      MOVE PH-BEN-PER-TIME-FCTR TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGADL  
00864                                                                   ELGADL  
00865      MOVE 'DEDL-BEN-PER-TIME-QUAL'  TO  CMF-ELEMENT-SYSTEM-NAME.  ELGADL  
00866      MOVE ACCUM-BEN-PER-TIME-QUAL   TO  CMF-CODE-VALUE.           ELGADL  
00867      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
00868                                                                   ELGADL  
00869 ************************************************************      ELGADL  
00870 *                                                          *      ELGADL  
00871 *    GENERATE INTERVAL TIME FACTOR TERM                    *      ELGADL  
00872 *                                                          *      ELGADL  
00873 ************************************************************      ELGADL  
00874  0330-GEN-INTRVL-TIME-FCTR-TRM.                                   ELGADL  
00875      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00876      MOVE PH-TIME-INTRVL-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGADL  
00877                                                                   ELGADL  
00878      MOVE ACCUM-INTERVAL-TIME-FCTR TO PH-INTRVL-TIME-FCTR.        ELGADL  
00879      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00880      MOVE PH-INTRVL-TIME-FCTR TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGADL  
00881                                                                   ELGADL  
00882      MOVE ACCUM-INTERVAL-TYPE   TO  CMF-CODE-VALUE.               ELGADL  
00883      MOVE 'DEDL-INTERVAL-TYPE'  TO  CMF-ELEMENT-SYSTEM-NAME.      ELGADL  
00884      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
00885                                                                   ELGADL  
00886 /***********************************************************      ELGADL  
00887 *                                                          *      ELGADL  
00888 *    GENERATE INTERVAL OVERRIDE NOTE                       *      ELGADL  
00889 *                                                          *      ELGADL  
00890 ************************************************************      ELGADL  
00891  0340-GEN-INTRVL-OVRD-NOTE.                                       ELGADL  
00892      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00893      MOVE PH-INTRVL-OVRD-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGADL  
00894                                                                   ELGADL  
00895      EVALUATE ACCUM-INTERVAL-OVRD-IND                             ELGADL  
00896         WHEN '1'   PERFORM 0341-GEN-INTRVL-OVRD-SPCL-PH-1         ELGADL  
00897         WHEN '2'   PERFORM 0342-GEN-INTRVL-OVRD-SPCL-PH-2         ELGADL  
00898         WHEN '3'   PERFORM 0343-GEN-INTRVL-OVRD-SPCL-PH-3         ELGADL  
00899         WHEN OTHER                                                ELGADL  
00900            MOVE ACCUM-INTERVAL-OVRD-IND TO CMF-CODE-VALUE         ELGADL  
00901            MOVE 'DEDL-INTERVAL-OVRD-IND'                          ELGADL  
00902              TO CMF-ELEMENT-SYSTEM-NAME                           ELGADL  
00903            PERFORM 0540-XLAT-ADL-CODE-VAL                         ELGADL  
00904         END-EVALUATE.                                             ELGADL  
00905                                                                   ELGADL  
00906      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00907      MOVE PH-INTRVL-OVRD-TRAIL TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGADL  
00908                                                                   ELGADL  
00909 ************************************************************      ELGADL  
00910 *                                                          *      ELGADL  
00911 *    GENERATE INTERVAL OVERRIDE SPECIAL PHRASE 1           *      ELGADL  
00912 *                                                          *      ELGADL  
00913 ************************************************************      ELGADL  
00914  0341-GEN-INTRVL-OVRD-SPCL-PH-1.                                  ELGADL  
00915      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00916      MOVE PH-INTRVL-OVRD-SPCL-1-LEAD                              ELGADL  
00917        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGADL  
00918                                                                   ELGADL  
00919      MOVE ACCUM-INTERVAL-OVRD-VALUE TO PH-INTRVL-OVRD-VAL.        ELGADL  
00920      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00921      MOVE PH-INTRVL-OVRD-VAL TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGADL  
00922                                                                   ELGADL  
00923      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00924      MOVE PH-INTRVL-OVRD-SPCL-1-TRAIL                             ELGADL  
00925        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGADL  
00926                                                                   ELGADL  
00927 ************************************************************      ELGADL  
00928 *                                                          *      ELGADL  
00929 *    GENERATE INTERVAL OVERRIDE SPECIAL PHRASE 2           *      ELGADL  
00930 *                                                          *      ELGADL  
00931 ************************************************************      ELGADL  
00932  0342-GEN-INTRVL-OVRD-SPCL-PH-2.                                  ELGADL  
00933      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00934      MOVE PH-INTRVL-OVRD-SPCL-2-LEAD                              ELGADL  
00935        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGADL  
00936                                                                   ELGADL  
00937      MOVE ACCUM-INTERVAL-OVRD-VALUE TO PH-INTRVL-OVRD-VAL.        ELGADL  
00938      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00939      MOVE PH-INTRVL-OVRD-VAL TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGADL  
00940                                                                   ELGADL  
00941      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00942      MOVE PH-INTRVL-OVRD-SPCL-2-TRAIL                             ELGADL  
00943        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGADL  
00944                                                                   ELGADL  
00945 ************************************************************      ELGADL  
00946 *                                                          *      ELGADL  
00947 *    GENERATE INTERVAL OVERRIDE SPECIAL PHRASE 3           *      ELGADL  
00948 *                                                          *      ELGADL  
00949 ************************************************************      ELGADL  
00950  0343-GEN-INTRVL-OVRD-SPCL-PH-3.                                  ELGADL  
00951      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00952      MOVE PH-INTRVL-OVRD-SPCL-3-LEAD                              ELGADL  
00953        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGADL  
00954                                                                   ELGADL  
00955      MOVE ACCUM-INTERVAL-OVRD-VALUE TO PH-INTRVL-OVRD-VAL.        ELGADL  
00956      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00957      MOVE PH-INTRVL-OVRD-VAL TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGADL  
00958                                                                   ELGADL  
00959      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00960      MOVE PH-INTRVL-OVRD-SPCL-3-TRAIL                             ELGADL  
00961        TO TCAR-FROM-LINE (TCAR-FROM-SUB).                         ELGADL  
00962                                                                   ELGADL  
00963 /***********************************************************      ELGADL  
00964 *                                                          *      ELGADL  
00965 *    GENERATE APPLICABILITY SENTENCE 1                     *      ELGADL  
00966 *                                                          *      ELGADL  
00967 ************************************************************      ELGADL  
00968  0350-GEN-APPLIC-SENT-1.                                          ELGADL  
00969      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00970      MOVE PH-APPLIC-1-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGADL  
00971                                                                   ELGADL  
00972      MOVE ACCUM-FAM-OR-INDIV   TO  CMF-CODE-VALUE.                ELGADL  
00973      MOVE 'DEDL-FAM-OR-INDIV'  TO  CMF-ELEMENT-SYSTEM-NAME.       ELGADL  
00974      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
00975                                                                   ELGADL  
00976      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00977      MOVE PH-WD-FOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGADL  
00978                                                                   ELGADL  
00979      MOVE ACCUM-L-O-B   TO  CMF-CODE-VALUE.                       ELGADL  
00980      MOVE 'DEDL-L-O-B'  TO  CMF-ELEMENT-SYSTEM-NAME.              ELGADL  
00981      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
00982                                                                   ELGADL  
00983      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00984      MOVE PH-WD-BENEFITS TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGADL  
00985                                                                   ELGADL  
00986      IF REINSTATEMENT-IND-NA                                      ELGADL  
00987      THEN                                                         ELGADL  
00988         CONTINUE                                                  ELGADL  
00989      ELSE                                                         ELGADL  
00990         PERFORM 0360-GEN-REINST-PH.                               ELGADL  
00991                                                                   ELGADL  
00992      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
00993      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGADL  
00994      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGADL  
00995                                                                   ELGADL  
00996 ************************************************************      ELGADL  
00997 *                                                          *      ELGADL  
00998 *    GENERATE REINSTATEMENT PHRASE                         *      ELGADL  
00999 *                                                          *      ELGADL  
01000 ************************************************************      ELGADL  
01001  0360-GEN-REINST-PH.                                              ELGADL  
01002      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01003      MOVE PH-REINST-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGADL  
01004                                                                   ELGADL  
01005      MOVE ACCUM-REINSTATEMENT-IND TO CMF-CODE-VALUE.              ELGADL  
01006      MOVE 'DEDL-REINSTATEMENT-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELGADL  
01007      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
01008                                                                   ELGADL  
01009 /***********************************************************      ELGADL  
01010 *                                                          *      ELGADL  
01011 *    GENERATE APPLICABILITY SENTENCE 2                     *      ELGADL  
01012 *                                                          *      ELGADL  
01013 ************************************************************      ELGADL  
01014  0370-GEN-APPLIC-SENT-2.                                          ELGADL  
01015      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01016      MOVE PH-WD-THIS TO TCAR-FROM-LINE (TCAR-FROM-SUB).           ELGADL  
01017                                                                   ELGADL  
01018      IF FYI-VALUE-NA                                              ELGADL  
01019      THEN                                                         ELGADL  
01020         CONTINUE                                                  ELGADL  
01021      ELSE                                                         ELGADL  
01022         PERFORM 0380-GEN-FYI-PH.                                  ELGADL  
01023                                                                   ELGADL  
01024      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01025      MOVE PH-APPLIC-2-LINK TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGADL  
01026                                                                   ELGADL  
01027      IF COST-CONTAIN-IND-NA                                       ELGADL  
01028      THEN                                                         ELGADL  
01029         ADD 1 TO TCAR-FROM-SUB                                    ELGADL  
01030         MOVE PH-WD-SERVICES TO TCAR-FROM-LINE (TCAR-FROM-SUB)     ELGADL  
01031      ELSE                                                         ELGADL  
01032         PERFORM 0390-GEN-COST-CONTAINMENT-PH.                     ELGADL  
01033                                                                   ELGADL  
01034      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01035      MOVE PH-WD-FOR TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGADL  
01036                                                                   ELGADL  
01037      PERFORM 0400-GEN-COND-BIT-PH.                                ELGADL  
01038                                                                   ELGADL  
01039      PERFORM 0410-GEN-PLC-OF-TRTMT-PH.                            ELGADL  
01040                                                                   ELGADL  
01041      EVALUATE      AGE-LMT-FROM-IND-NA                            ELGADL  
01042               ALSO AGE-LMT-TO-IND-NA                              ELGADL  
01043               ALSO RELATIONSHIP-IND-NA                            ELGADL  
01044         WHEN TRUE ALSO TRUE ALSO TRUE                             ELGADL  
01045            CONTINUE                                               ELGADL  
01046         WHEN TRUE ALSO TRUE ALSO FALSE                            ELGADL  
01047            PERFORM 0420-GEN-PT-RLTNSHP-PH                         ELGADL  
01048         WHEN OTHER                                                ELGADL  
01049            PERFORM 0430-GEN-PT-RLTNSHP-AGE-PH                     ELGADL  
01050         END-EVALUATE.                                             ELGADL  
01051                                                                   ELGADL  
01052      IF SW-HAS-INTERNALS                                          ELGADL  
01053      THEN                                                         ELGADL  
01054         PERFORM 0460-GEN-INTRNL-TAB-LST                           ELGADL  
01055      ELSE                                                         ELGADL  
01056         CONTINUE.                                                 ELGADL  
01057                                                                   ELGADL  
01058      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01059      MOVE '.' TO TCAR-FROM-LINE (TCAR-FROM-SUB).                  ELGADL  
01060                                                                   ELGADL  
01061      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGADL  
01062                                                                   ELGADL  
01063 /***********************************************************      ELGADL  
01064 *                                                          *      ELGADL  
01065 *    GENERATE FYI PHRASE                                   *      ELGADL  
01066 *                                                          *      ELGADL  
01067 ************************************************************      ELGADL  
01068  0380-GEN-FYI-PH.                                                 ELGADL  
01069      MOVE ACCUM-FYI-VALUE   TO  CMF-CODE-VALUE.                   ELGADL  
01070      MOVE 'DEDL-FYI-VALUE'  TO  CMF-ELEMENT-SYSTEM-NAME.          ELGADL  
01071      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
01072                                                                   ELGADL  
01073 ************************************************************      ELGADL  
01074 *                                                          *      ELGADL  
01075 *    GENERATE COST CONTAINMENT PHRASE                      *      ELGADL  
01076 *                                                          *      ELGADL  
01077 ************************************************************      ELGADL  
01078  0390-GEN-COST-CONTAINMENT-PH.                                    ELGADL  
01079      MOVE ACCUM-COST-CONTAIN-IND   TO  CMF-CODE-VALUE.            ELGADL  
01080      MOVE 'DEDL-COST-CONTAIN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELGADL  
01081      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
01082                                                                   ELGADL  
01083 ************************************************************      ELGADL  
01084 *                                                          *      ELGADL  
01085 *    GENERATE CONDITION BITS PHRASE                        *      ELGADL  
01086 *                                                          *      ELGADL  
01087 ************************************************************      ELGADL  
01088  0400-GEN-COND-BIT-PH.                                            ELGADL  
01089                                                                   ELGADL  
01090 * -- PRESERVE CONTENTS OF TCAR-FROM-AREA                          ELGADL  
01091 *    (ELUCONDB USES THE TEXT COMPRESSION WORK AREA, WHICH CAUSES  ELGADL  
01092 *    THE LOSS OF ANYTHING IN TCAR-FROM-AREA.  WE MUST SAVE THE    ELGADL  
01093 *    CURRENT CONTENT OF TCAR-FROM-AREA AND RESTORE IT AFTER       ELGADL  
01094 *    OBTAINING THE TRANSLATION OF THE CONDITION BITS.)            ELGADL  
01095      MOVE TCAR-FROM-SUB TO WS-TEXT-HOLD-COUNT.                    ELGADL  
01096      MOVE TCAR-FROM-AREA TO WS-TEXT-HOLD-TEXT.                    ELGADL  
01097                                                                   ELGADL  
01098 * -- GET CONDITION BIT TRANSLATION                                ELGADL  
01099      MOVE ACCUM-CONDITION  TO  CMF-CONDITION-BITS.                ELGADL  
01100      CALL 'ELUCONDB' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADL  
01101                                                                   ELGADL  
01102      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGADL  
01103      CALL 'ELUSETAD'                                              ELGADL  
01104         USING DFHCOMMAREA                                         ELGADL  
01105               ADDRESS OF CMF-DESCR.                               ELGADL  
01106                                                                   ELGADL  
01107 * -- RESTORE THE CONTENTS OF TCAR-FROM-AREA                       ELGADL  
01108      MOVE WS-TEXT-HOLD-COUNT TO TCAR-FROM-SUB.                    ELGADL  
01109      MOVE WS-TEXT-HOLD-TEXT TO TCAR-FROM-AREA.                    ELGADL  
01110                                                                   ELGADL  
01111 * -- APPEND THE TRANSLATION                                       ELGADL  
01112      PERFORM 0550-MOVE-TRANSLATION-TO-COMPR.                      ELGADL  
01113                                                                   ELGADL  
01114 /***********************************************************      ELGADL  
01115 *                                                          *      ELGADL  
01116 *    GENERATE PLACE OF TREATMENT PHRASE                    *      ELGADL  
01117 *                                                          *      ELGADL  
01118 ************************************************************      ELGADL  
01119  0410-GEN-PLC-OF-TRTMT-PH.                                        ELGADL  
01120      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01121      MOVE PH-WD-PROVIDED TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGADL  
01122                                                                   ELGADL  
01123      MOVE ACCUM-PLACE-OF-TREATMENT  TO  CMF-CODE-VALUE.           ELGADL  
01124      MOVE 'DEDL-PLACE-OF-TREATMENT' TO  CMF-ELEMENT-SYSTEM-NAME.  ELGADL  
01125      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
01126                                                                   ELGADL  
01127 ************************************************************      ELGADL  
01128 *                                                          *      ELGADL  
01129 *    GENERATE PATIENT RELATIONSHIP PHRASE                  *      ELGADL  
01130 *                                                          *      ELGADL  
01131 ************************************************************      ELGADL  
01132  0420-GEN-PT-RLTNSHP-PH.                                          ELGADL  
01133      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01134      MOVE PH-WD-TO TO TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGADL  
01135                                                                   ELGADL  
01136      MOVE ACCUM-RELATIONSHIP-IND   TO  CMF-CODE-VALUE.            ELGADL  
01137      MOVE 'DEDL-RELATIONSHIP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELGADL  
01138      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
01139                                                                   ELGADL  
01140 /***********************************************************      ELGADL  
01141 *                                                          *      ELGADL  
01142 *    GENERATE PATIENT RELATIONSHIP AND AGE PHRASE          *      ELGADL  
01143 *                                                          *      ELGADL  
01144 ************************************************************      ELGADL  
01145  0430-GEN-PT-RLTNSHP-AGE-PH.                                      ELGADL  
01146      IF RELATIONSHIP-IND-NA                                       ELGADL  
01147      THEN                                                         ELGADL  
01148         ADD 1 TO TCAR-FROM-SUB                                    ELGADL  
01149         MOVE PH-NO-PT-RLTNSHP TO TCAR-FROM-LINE (TCAR-FROM-SUB)   ELGADL  
01150      ELSE                                                         ELGADL  
01151         PERFORM 0420-GEN-PT-RLTNSHP-PH.                           ELGADL  
01152                                                                   ELGADL  
01153      PERFORM 0440-GEN-FROM-AGE-PH.                                ELGADL  
01154                                                                   ELGADL  
01155      PERFORM 0450-GEN-TO-AGE-PH.                                  ELGADL  
01156                                                                   ELGADL  
01157 /***********************************************************      ELGADL  
01158 *                                                          *      ELGADL  
01159 *    GENERATE FROM AGE PHRASE                              *      ELGADL  
01160 *                                                          *      ELGADL  
01161 ************************************************************      ELGADL  
01162  0440-GEN-FROM-AGE-PH.                                            ELGADL  
01163      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01164      MOVE PH-PT-FROM-AGE TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELGADL  
01165                                                                   ELGADL  
01166      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01167      IF ACCUM-AGE-LIMIT-FROM-UNLIM                                ELGADL  
01168      THEN                                                         ELGADL  
01169         MOVE PH-WD-UNLIMITED TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELGADL  
01170      ELSE                                                         ELGADL  
01171         MOVE ACCUM-AGE-LIMIT-FROM-VAL TO PH-AGE-LIM-FROM          ELGADL  
01172         MOVE PH-AGE-LIM-FROM TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGADL  
01173                                                                   ELGADL  
01174      MOVE ACCUM-AGE-LIMIT-FROM-IND  TO  CMF-CODE-VALUE.           ELGADL  
01175      MOVE 'DEDL-AGE-QUAL-IND-FROM'  TO  CMF-ELEMENT-SYSTEM-NAME.  ELGADL  
01176      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
01177                                                                   ELGADL  
01178 ************************************************************      ELGADL  
01179 *                                                          *      ELGADL  
01180 *    GENERATE TO AGE PHRASE                                *      ELGADL  
01181 *                                                          *      ELGADL  
01182 ************************************************************      ELGADL  
01183  0450-GEN-TO-AGE-PH.                                              ELGADL  
01184      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01185      MOVE PH-PT-TO-AGE TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELGADL  
01186                                                                   ELGADL  
01187      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01188      IF ACCUM-AGE-LIMIT-TO-UNLIM                                  ELGADL  
01189      THEN                                                         ELGADL  
01190         MOVE PH-WD-UNLIMITED TO TCAR-FROM-LINE (TCAR-FROM-SUB)    ELGADL  
01191      ELSE                                                         ELGADL  
01192         MOVE ACCUM-AGE-LIMIT-TO-VAL TO PH-AGE-LIM-TO              ELGADL  
01193         MOVE PH-AGE-LIM-TO TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGADL  
01194                                                                   ELGADL  
01195      MOVE ACCUM-AGE-LIMIT-TO-IND    TO  CMF-CODE-VALUE.           ELGADL  
01196      MOVE 'DEDL-AGE-QUAL-IND-TO'    TO  CMF-ELEMENT-SYSTEM-NAME.  ELGADL  
01197      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
01198                                                                   ELGADL  
01199 /***********************************************************      ELGADL  
01200 *                                                          *      ELGADL  
01201 *    GENERATE INTERNAL TABULARS LIST                       *      ELGADL  
01202 *                                                          *      ELGADL  
01203 ************************************************************      ELGADL  
01204  0460-GEN-INTRNL-TAB-LST.                                         ELGADL  
01205      INITIALIZE WS-INT-TAB-LST                                    ELGADL  
01206                 WS-INT-TAB-TXT                                    ELGADL  
01207                 WS-TAB-SUB.                                       ELGADL  
01208                                                                   ELGADL  
01209      IF SW-HAS-IBGR                                               ELGADL  
01210         ADD 1 TO WS-TAB-SUB                                       ELGADL  
01211         SET WS-INT-TAB-IBGR (WS-TAB-SUB)                          ELGADL  
01212             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGADL  
01213                                                                   ELGADL  
01214      IF SW-HAS-IDGD                                               ELGADL  
01215         ADD 1 TO WS-TAB-SUB                                       ELGADL  
01216         SET WS-INT-TAB-IDGD (WS-TAB-SUB)                          ELGADL  
01217             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGADL  
01218                                                                   ELGADL  
01219      IF SW-HAS-IPGN                                               ELGADL  
01220         ADD 1 TO WS-TAB-SUB                                       ELGADL  
01221         SET WS-INT-TAB-IPGN (WS-TAB-SUB)                          ELGADL  
01222             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGADL  
01223                                                                   ELGADL  
01224      IF SW-HAS-IPGP                                               ELGADL  
01225         ADD 1 TO WS-TAB-SUB                                       ELGADL  
01226         SET WS-INT-TAB-IPGP (WS-TAB-SUB)                          ELGADL  
01227             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGADL  
01228                                                                   ELGADL  
01229      IF SW-HAS-IPGT                                               ELGADL  
01230         ADD 1 TO WS-TAB-SUB                                       ELGADL  
01231         SET WS-INT-TAB-IPGT (WS-TAB-SUB)                          ELGADL  
01232             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGADL  
01233                                                                   ELGADL  
01234      IF SW-HAS-IPGS                                               ELGADL  
01235         ADD 1 TO WS-TAB-SUB                                       ELGADL  
01236         SET WS-INT-TAB-IPGS (WS-TAB-SUB)                          ELGADL  
01237             WS-INT-TAB-COMMA (WS-TAB-SUB) TO TRUE.                ELGADL  
01238                                                                   ELGADL  
01239      IF WS-TAB-SUB > 0                                            ELGADL  
01240         SET WS-INT-TAB-END (WS-TAB-SUB) TO TRUE                   ELGADL  
01241         IF  WS-TAB-SUB > 1                                        ELGADL  
01242         THEN                                                      ELGADL  
01243            SET WS-INT-TAB-AND (WS-TAB-SUB - 1) TO TRUE            ELGADL  
01244         END-IF                                                    ELGADL  
01245         ADD 1 TO TCAR-FROM-SUB                                    ELGADL  
01246         MOVE PH-INTRNL-TAB-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB) ELGADL  
01247         ADD 1 TO TCAR-FROM-SUB                                    ELGADL  
01248         MOVE WS-INT-TAB-TXT-1 TO TCAR-FROM-LINE (TCAR-FROM-SUB)   ELGADL  
01249         IF WS-TAB-SUB > 3                                         ELGADL  
01250         THEN                                                      ELGADL  
01251            ADD 1 TO TCAR-FROM-SUB                                 ELGADL  
01252            MOVE WS-INT-TAB-TXT-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB)ELGADL  
01253         END-IF                                                    ELGADL  
01254         ADD 1 TO TCAR-FROM-SUB                                    ELGADL  
01255         MOVE PH-INTRNL-TAB-TRAIL TO TCAR-FROM-LINE (TCAR-FROM-SUB)ELGADL  
01256      END-IF.                                                      ELGADL  
01257                                                                   ELGADL  
01258 /***********************************************************      ELGADL  
01259 *                                                          *      ELGADL  
01260 *    GENERATE DEFINITION SENTENCE                          *      ELGADL  
01261 *                                                          *      ELGADL  
01262 ************************************************************      ELGADL  
01263  0510-GEN-DEFN-SENT.                                              ELGADL  
01264      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01265      MOVE PH-DEFN-LEAD TO TCAR-FROM-LINE (TCAR-FROM-SUB).         ELGADL  
01266                                                                   ELGADL  
01267      MOVE ACCUM-DEFINITION      TO CMF-CODE-VALUE.                ELGADL  
01268      MOVE 'DEDL-DEFINITION'     TO CMF-ELEMENT-SYSTEM-NAME.       ELGADL  
01269      PERFORM 0540-XLAT-ADL-CODE-VAL.                              ELGADL  
01270                                                                   ELGADL  
01271      ADD 1 TO TCAR-FROM-SUB.                                      ELGADL  
01272      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGADL  
01273                                                                   ELGADL  
01274      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGADL  
01275                                                                   ELGADL  
01276 /***********************************************************      ELGADL  
01277 *                                                          *      ELGADL  
01278 *    GENERATE TOPIC DEDUCTIBLE OCCURRENCE TRAILER          *      ELGADL  
01279 *                                                          *      ELGADL  
01280 ************************************************************      ELGADL  
01281  0520-GEN-TOPIC-TRAILER.                                          ELGADL  
01282      IF TCAR-FROM-SUB > 0                                         ELGADL  
01283      THEN                                                         ELGADL  
01284         PERFORM 0610-COMPLETE-AND-SEND-PARA.                      ELGADL  
01285      ADD 1 TO TCAR-FROM-SUB                                       ELGADL  
01286      MOVE PH-SEE-BP-TOPICS-1 TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGADL  
01287      ADD 1 TO TCAR-FROM-SUB                                       ELGADL  
01288      MOVE PH-SEE-BP-TOPICS-2 TO TCAR-FROM-LINE (TCAR-FROM-SUB).   ELGADL  
01289      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGADL  
01290                                                                   ELGADL  
01291 ************************************************************      ELGADL  
01292 *                                                          *      ELGADL  
01293 *    GENERATE BENEFIT PROVISION DED OCCURRENCE TRAILER     *      ELGADL  
01294 *                                                          *      ELGADL  
01295 ************************************************************      ELGADL  
01296  0530-GEN-BP-TRAILER.                                             ELGADL  
01297      IF TCAR-FROM-SUB > 0                                         ELGADL  
01298      THEN                                                         ELGADL  
01299         PERFORM 0610-COMPLETE-AND-SEND-PARA.                      ELGADL  
01300      ADD 1 TO TCAR-FROM-SUB                                       ELGADL  
01301      MOVE PH-SEE-DED-TOPIC TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELGADL  
01302      PERFORM 0610-COMPLETE-AND-SEND-PARA.                         ELGADL  
01303                                                                   ELGADL  
01304 /***********************************************************      ELGADL  
01305 *                                                          *      ELGADL  
01306 *    TRANSLATE ACCUMULATOR CODE VALUE                      *      ELGADL  
01307 *    AND MOVE TO COMPRESS WORK AREA                        *      ELGADL  
01308 *                                                          *      ELGADL  
01309 ************************************************************      ELGADL  
01310  0540-XLAT-ADL-CODE-VAL.                                          ELGADL  
01311      MOVE '#ADL' TO CMF-RECORD-PREFIX.                            ELGADL  
01312      EXEC CICS LINK PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)      ELGADL  
01313         END-EXEC.                                                 ELGADL  
01314                                                                   ELGADL  
01315      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGADL  
01316      CALL 'ELUSETAD'                                              ELGADL  
01317         USING DFHCOMMAREA                                         ELGADL  
01318               ADDRESS OF CMF-DESCR.                               ELGADL  
01319                                                                   ELGADL  
01320      PERFORM 0550-MOVE-TRANSLATION-TO-COMPR.                      ELGADL  
01321                                                                   ELGADL  
01322 ************************************************************      ELGADL  
01323 *                                                          *      ELGADL  
01324 *        MOVE TRANSLATION TO COMPRESS AREA                 *      ELGADL  
01325 *                                                          *      ELGADL  
01326 ************************************************************      ELGADL  
01327  0550-MOVE-TRANSLATION-TO-COMPR.                                  ELGADL  
01328      PERFORM WITH TEST BEFORE                                     ELGADL  
01329            VARYING CMF-DESCR-IDX FROM 1 BY 1                      ELGADL  
01330              UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES            ELGADL  
01331         IF TCAR-FROM-SUB >= 20                                    ELGADL  
01332         THEN                                                      ELGADL  
01333            PERFORM 0600-SEND-PART-PARA                            ELGADL  
01334         END-IF                                                    ELGADL  
01335         ADD 1 TO TCAR-FROM-SUB                                    ELGADL  
01336         MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                       ELGADL  
01337           TO TCAR-FROM-LINE (TCAR-FROM-SUB)                       ELGADL  
01338         END-PERFORM.                                              ELGADL  
01339                                                                   ELGADL  
01340 /***********************************************************      ELGADL  
01341 *                                                          *      ELGADL  
01342 *    SEND A PARTIAL PARAGRAPH TO OUTPUT                    *      ELGADL  
01343 *                                                          *      ELGADL  
01344 ************************************************************      ELGADL  
01345  0600-SEND-PART-PARA.                                             ELGADL  
01346 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGADL  
01347      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGADL  
01348 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGADL  
01349      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGADL  
01350      MOVE +79                                                     ELGADL  
01351        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGADL  
01352           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGADL  
01353           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGADL  
01354           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGADL  
01355           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGADL  
01356           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGADL  
01357           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGADL  
01358           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGADL  
01359           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGADL  
01360           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGADL  
01361 * -- UNSTRING/FLOW THE OUTPUT                                     ELGADL  
01362      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGADL  
01363 * -- MOVE FORMATTED TEXT TO OUTPUT ** EXCEPT LAST LINE **         ELGADL  
01364      PERFORM WITH TEST BEFORE                                     ELGADL  
01365            VARYING TCAR-X FROM 1 BY 1                             ELGADL  
01366              UNTIL TCAR-X = TCAR-OUTPUT-FIELDS-USED               ELGADL  
01367 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGADL  
01368         IF COF-NBR-DTL-LINES >= 20                                ELGADL  
01369         THEN                                                      ELGADL  
01370            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGADL  
01371            INITIALIZE COF-DTL                                     ELGADL  
01372            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGADL  
01373         END-IF                                                    ELGADL  
01374 *    -- APPEND LINE TO OUTPUT                                     ELGADL  
01375         ADD 1 TO COF-NBR-DTL-LINES                                ELGADL  
01376         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGADL  
01377           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGADL  
01378         END-PERFORM.                                              ELGADL  
01379 * -- PUT LAST LINE OF COMPRESSED/UNSTRUNG OUTPUT INTO FROM AREA   ELGADL  
01380      INITIALIZE TCAR-FROM-AREA                                    ELGADL  
01381                 TCAR-FROM-LENGTH                                  ELGADL  
01382                 TCAR-FROM-SUB.                                    ELGADL  
01383      MOVE 1 TO TCAR-FROM-SUB.                                     ELGADL  
01384      MOVE TCAR-OPF-DATA (TCAR-OUTPUT-FIELDS-USED)                 ELGADL  
01385        TO TCAR-FROM-LINE (1).                                     ELGADL  
01386                                                                   ELGADL  
01387 /***********************************************************      ELGADL  
01388 *                                                          *      ELGADL  
01389 *    COMPLETE AND SEND A PARAGRAPH TO OUTPUT               *      ELGADL  
01390 *                                                          *      ELGADL  
01391 ************************************************************      ELGADL  
01392  0610-COMPLETE-AND-SEND-PARA.                                     ELGADL  
01393 * -- COMPRESS TEXT IN COMPRESSION AREA                            ELGADL  
01394      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGADL  
01395 * -- SETUP FOR UNSTRING/WORD FLOW                                 ELGADL  
01396      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGADL  
01397      MOVE +79                                                     ELGADL  
01398        TO TCAR-OUTPUT-FIELD-1-LEN  TCAR-OUTPUT-FIELD-2-LEN        ELGADL  
01399           TCAR-OUTPUT-FIELD-3-LEN  TCAR-OUTPUT-FIELD-4-LEN        ELGADL  
01400           TCAR-OUTPUT-FIELD-5-LEN  TCAR-OUTPUT-FIELD-6-LEN        ELGADL  
01401           TCAR-OUTPUT-FIELD-7-LEN  TCAR-OUTPUT-FIELD-8-LEN        ELGADL  
01402           TCAR-OUTPUT-FIELD-9-LEN  TCAR-OUTPUT-FIELD-10-LEN       ELGADL  
01403           TCAR-OUTPUT-FIELD-11-LEN TCAR-OUTPUT-FIELD-12-LEN       ELGADL  
01404           TCAR-OUTPUT-FIELD-13-LEN TCAR-OUTPUT-FIELD-14-LEN       ELGADL  
01405           TCAR-OUTPUT-FIELD-15-LEN TCAR-OUTPUT-FIELD-16-LEN       ELGADL  
01406           TCAR-OUTPUT-FIELD-17-LEN TCAR-OUTPUT-FIELD-18-LEN       ELGADL  
01407           TCAR-OUTPUT-FIELD-19-LEN TCAR-OUTPUT-FIELD-20-LEN       ELGADL  
01408 * -- UNSTRING/FLOW THE OUTPUT                                     ELGADL  
01409      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGADL  
01410 * -- MOVE FORMATTED TEXT TO OUTPUT                                ELGADL  
01411      PERFORM WITH TEST AFTER                                      ELGADL  
01412            VARYING TCAR-X FROM 1 BY 1                             ELGADL  
01413              UNTIL TCAR-X > TCAR-OUTPUT-FIELDS-USED               ELGADL  
01414 *    -- SEND THE OUTPUT BUFFER IF FULL                            ELGADL  
01415         IF COF-NBR-DTL-LINES >= 20                                ELGADL  
01416         THEN                                                      ELGADL  
01417            CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL    ELGADL  
01418            INITIALIZE COF-DTL                                     ELGADL  
01419            MOVE ZERO TO COF-NBR-DTL-LINES                         ELGADL  
01420         END-IF                                                    ELGADL  
01421 *    -- APPEND LINE TO OUTPUT                                     ELGADL  
01422         ADD 1 TO COF-NBR-DTL-LINES                                ELGADL  
01423         MOVE TCAR-OPF-DATA (TCAR-X)                               ELGADL  
01424           TO COF-DTL-LINE (COF-NBR-DTL-LINES)                     ELGADL  
01425         END-PERFORM.                                              ELGADL  
01426                                                                   ELGADL  
01427 * -- CLEAR THE COMPRESSION WORK AREA                              ELGADL  
01428      INITIALIZE TCAR-FROM-AREA                                    ELGADL  
01429                 TCAR-FROM-SUB.                                    ELGADL  
01430 * -- INSERT A BLANK LINE                                          ELGADL  
01431      ADD 1 TO COF-NBR-DTL-LINES.                                  ELGADL  
01432      MOVE SPACES TO COF-DTL-LINE (COF-NBR-DTL-LINES).             ELGADL  
01433 * -- CALL THE OUTPUT MODULE                                       ELGADL  
01434      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA.                  ELGADL  
01435      INITIALIZE COF-DTL.                                          ELGADL  
01436      MOVE ZERO TO COF-NBR-DTL-LINES.                              ELGADL  
01437                                                                   ELGADL  
01438 /***********************************************************      ELGADL  
01439 *                                                          *      ELGADL  
01440 *    GENERATE INTERNAL TABULAR LISTINGS                    *      ELGADL  
01441 *                                                          *      ELGADL  
01442 ************************************************************      ELGADL  
01443  0650-GEN-INTRNL-TAB-LSTNGS.                                      ELGADL  
01444                                                                   ELGADL  
01445      IF SW-HAS-IBGR                                               ELGADL  
01446         MOVE PC-IBGR TO SRP-INTERNAL-TAB-ID                       ELGADL  
01447         EXEC CICS LINK PROGRAM ('ELGIBGR')                        ELGADL  
01448                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGADL  
01449                                                                   ELGADL  
01450      IF SW-HAS-IDGD                                               ELGADL  
01451         MOVE PC-IDGD TO SRP-INTERNAL-TAB-ID                       ELGADL  
01452         EXEC CICS LINK PROGRAM ('ELGIDGD')                        ELGADL  
01453                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGADL  
01454                                                                   ELGADL  
01455      IF SW-HAS-IPGN                                               ELGADL  
01456         MOVE PC-IPGN TO SRP-INTERNAL-TAB-ID                       ELGADL  
01457         EXEC CICS LINK PROGRAM ('ELGIPGN')                        ELGADL  
01458                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGADL  
01459                                                                   ELGADL  
01460      IF SW-HAS-IPGP                                               ELGADL  
01461         MOVE PC-IPGP TO SRP-INTERNAL-TAB-ID                       ELGADL  
01462         EXEC CICS LINK PROGRAM ('ELGIPGP')                        ELGADL  
01463                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGADL  
01464                                                                   ELGADL  
01465      IF SW-HAS-IPGT                                               ELGADL  
01466         MOVE PC-IPGT TO SRP-INTERNAL-TAB-ID                       ELGADL  
01467         EXEC CICS LINK PROGRAM ('ELGIPGT')                        ELGADL  
01468                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGADL  
01469                                                                   ELGADL  
01470      IF SW-HAS-IPGS                                               ELGADL  
01471         MOVE PC-IPGS TO SRP-INTERNAL-TAB-ID                       ELGADL  
01472         EXEC CICS LINK PROGRAM ('ELGIPGS')                        ELGADL  
01473                        COMMAREA (DFHCOMMAREA) END-EXEC.           ELGADL  
01474                                                                   ELGADL  
01475 /***********************************************************      ELGADL  
01476 *                                                          *      ELGADL  
01477 *    READ ACCUMULATOR WORK FILE RECORD                     *      ELGADL  
01478 *                                                          *      ELGADL  
01479 ************************************************************      ELGADL  
01480                                                                   ELGADL  
01481  0660-READ-ACCUM-WORK-FILE-REC.                                   ELGADL  
01482      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGADL  
01483      CALL 'ELUSETAD'                                              ELGADL  
01484          USING DFHCOMMAREA                                        ELGADL  
01485                ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.            ELGADL  
01486      SET IOP-FCQ-NONE         TO  TRUE.                           ELGADL  
01487      SET IOP-KVQ-NONE         TO  TRUE.                           ELGADL  
01488      SET IOP-STG-MODE-LOCATE  TO  TRUE.                           ELGADL  
01489      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADL  
01490      SET ADDRESS OF ACCUM-OCCURENCE-SUMMARY TO IOP-REC-PTR.       ELGADL  
01491                                                                   ELGADL  
01492 ************************************************************      ELGADL  
01493 *                                                          *      ELGADL  
01494 *    DELETE ACCUMULATOR WORK FILE                          *      ELGADL  
01495 *                                                          *      ELGADL  
01496 ************************************************************      ELGADL  
01497                                                                   ELGADL  
01498  0670-DELETE-ACCUM-WORK-FILE.                                     ELGADL  
01499      SET CIA-ELSWKFL1-DDN TO TRUE.                                ELGADL  
01500      CALL 'ELUSETAD'                                              ELGADL  
01501         USING DFHCOMMAREA                                         ELGADL  
01502               ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.             ELGADL  
01503      SET  IOP-DEL           TO  TRUE.                             ELGADL  
01504      SET  IOP-FCQ-NONE      TO  TRUE.                             ELGADL  
01505      SET  IOP-KVQ-NONE      TO  TRUE.                             ELGADL  
01506      CALL 'ELUIOPGM' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADL  
01507                                                                   ELGADL  
01508 /***********************************************************      ELGADL  
01509 *                                                          *      ELGADL  
01510 *        END THE DISPLAY                                   *      ELGADL  
01511 *                                                          *      ELGADL  
01512 ************************************************************      ELGADL  
01513  0680-END-THE-DISPLAY.                                            ELGADL  
01514      IF COF-NBR-DTL-LINES > 0                                     ELGADL  
01515         CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.      ELGADL  
01516      SET COF-END  TO  TRUE.                                       ELGADL  
01517      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELGADL  
01518      MOVE +0      TO  COF-NBR-DTL-LINES.                          ELGADL  
01519      CALL 'ELUOUTPT' USING DFHEIBLK DFHCOMMAREA END-CALL.         ELGADL  
