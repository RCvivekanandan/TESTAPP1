00001 *      LAST MAINTENANCE TIME: 14.14.33  DATE: 08/18/89            09/03/03
00002 * STRUCTURE(S) MEMBER ELGCSIHSPL - LEVEL 077 AS OF 11/30/88       ELGCSIHS
00003 * FROM PANLIB R360059.STRUCTPL.PANLIB                                LV002
00004 *                                                                 ELGCSIHS
00005  IDENTIFICATION DIVISION.                                         ELGCSIHS
00006                                                                   ELGCSIHS
00007  PROGRAM-ID.         ELGCSIHS.                                    ELGCSIHS
00008                                                                   ELGCSIHS
00009  AUTHOR.             RICK BARILEAU.                               ELGCSIHS
00010                                                                   ELGCSIHS
00011  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGCSIHS
00012                      A MUTUAL LEGAL RESERVE COMPANY               ELGCSIHS
00013                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGCSIHS
00014                      233 N. MICHIGAN AVE                          ELGCSIHS
00015                      CHICAGO, ILLINOIS 60601                      ELGCSIHS
00016                                                                   ELGCSIHS
00017  DATE-WRITTEN.       07-JAN-1988.                                 ELGCSIHS
00018                                                                   ELGCSIHS
00019  DATE-COMPILED.                                                   ELGCSIHS
00020                                                                   ELGCSIHS
00021  SECURITY.           COPYRIGHT 1986,                              ELGCSIHS
00022                      HEALTH CARE SERVICE CORPORATION              ELGCSIHS
00023      SKIP3                                                        ELGCSIHS
00024  ENVIRONMENT DIVISION.                                            ELGCSIHS
00025                                                                   ELGCSIHS
00026  CONFIGURATION SECTION.                                           ELGCSIHS
00027  SOURCE-COMPUTER.    IBM-3090.                                    ELGCSIHS
00028  OBJECT-COMPUTER.    IBM-3090.                                    ELGCSIHS
00029      EJECT                                                        ELGCSIHS
00030 ******************************************************************ELGCSIHS
00031 *                                                                *ELGCSIHS
00032 *  ELGCSIHS - ELS:  THIS MODULE GATHERS ALL INFORMATION THAT     *ELGCSIHS
00033 *                   PERTAINS TO INPATIENT HOSPITAL SERVICES WITH *ELGCSIHS
00034 *                   THE CONTRACT AND WILL DISPLAY IT.            *ELGCSIHS
00035 *                                                                *ELGCSIHS
00036 *  NOTES :  THE ACCUMULATORS WILL BE USED AT ALL THREE LEVELS    *ELGCSIHS
00037 *           (BENEFIT PROVISION, CONTRACT, GROUP SPECIFIC) TO     *ELGCSIHS
00038 *           OBTAIN THE INFO IT REQUIRES.                         *ELGCSIHS
00039 *                                                                *ELGCSIHS
00040 ******************************************************************ELGCSIHS
00041 *                                                                *ELGCSIHS
00042 *                      MAINTENANCE HISTORY                       *ELGCSIHS
00043 *                                                                *ELGCSIHS
00044 *  MOD     DATE     BY  DRPT                ACTION               *ELGCSIHS
00045 * ----- ----------- --- ----- ---------------------------------- *ELGCSIHS
00046 * 01.00 07-JAN-1988 REB       CREATED                            *ELGCSIHS
00047 *                                                                *ELGCSIHS
00048 * 01.01 20-JAN-1988 REB       MADE SOME CHANGES TO UPDATED C.S.  *ELGCSIHS
00049 *                             COPYBOOKS.                         *ELGCSIHS
00050 *                                                                *ELGCSIHS
00051 * 01.02 29-JAN-1988 REB       ADDED CODE TO EXTRACT INFO FROM    *ELGCSIHS
00052 *                             'ELUCSCOV'.                        *ELGCSIHS
00053 *                                                                *ELGCSIHS
00054 * 01.03 09-FEB-1988 REB       MADE CHANGES FOR COPYBOOKS AND     *ELGCSIHS
00055 *                             PUT IN LOGIC FOR BENEFIT AND       *ELGCSIHS
00056 *                             INTERVAL LINES.                    *ELGCSIHS
00057 *                                                                *ELGCSIHS
00058 * 01.04 04-MAR-1988 REB       REMOVED LOGIC TO READ IN #ABM FOR  *ELGCSIHS
00059 *                             GROUP AND CONTRACT ALREADY READ IN.*ELGCSIHS
00060 *                                                                *ELGCSIHS
00061 * 01.05 14-MAR-1988 REB       FIXING THE WAY EDITED FIELDS ARE   *ELGCSIHS
00062 *                             DISPLAYED.                         *ELGCSIHS
00063 *                                                                *ELGCSIHS
00064 * 01.06 25-MAR-1988 REB       FIXED LOGIC TO DISPLAY CHC DAYS.   *ELGCSIHS
00065 *                                                                *ELGCSIHS
00066 * 01.07 29-MAR-1988 REB       SPEC CHANGE FOR PROCESSING ON      *ELGCSIHS
00067 *                             MENT,DRUG,ALC DAYS. ANOTHER        *ELGCSIHS
00068 *                             QUALIFIER TO BE MET AND IF MEDICARE*ELGCSIHS
00069 *                             CHOSEN KEY ON 'MCOI A' PROVISION.  *ELGCSIHS
00070 *                                                                *ELGCSIHS
00071 * 01.08 30-MAR-1988 REB       SPEC CHANGE FOR QUALIFYING THE     *ELGCSIHS
00072 *                             BASIC DAYS. NOW MUST READ IBGR TO  *ELGCSIHS
00073 *                             VERIFY THAT PROVISION IS INCLUDED. *ELGCSIHS
00074 *                                                                *ELGCSIHS
00075 * 01.09 05-APR-1988 REB       TEXT CHANGE FOR BENEFIT AND        *ELGCSIHS
00076 *                             INTERVAL HEADINGS.                 *ELGCSIHS
00077 *                                                                *ELGCSIHS
00078 * 01.10 19-OCT-1988 EGL       1.  CHANGED TO USE NEW STORAGE     *ELGCSIHS
00079 *                               MANAGEMENT ROUTINES.             *ELGCSIHS
00080 *                             2.  CHANGED METHOD OF DETERMING    *ELGCSIHS
00081 *                               NUMBER OF INPATIENT DAYS.        *ELGCSIHS
00082 *                                                                *ELGCSIHS
00083 * 01.11 21-AUG-1989 AKK       DESTRUCTED PROGRAM.                *ELGCSIHS
00084 *                                                                *ELGCSIHS
00085 * 01.12 27-SEP-1991 JPB       EXPANDED REFERENCES TO FAMILY-REL  *ELGCSIHS
00086 *                             FROM 1 TO 2 BYTES.                 *ELGCSIHS
00087 * 01.13 02-OCT-1991 JPB       ADDED PC-FAM-REL-LVL-MEDIC CONSTANT*ELGCSIHS
00088 * 01.14 10-OCT-1991 JPB       CHANGED GC#IBGR TO GCTIBGR.        *ELGCSIHS
00089 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGCSIHS
00090 ******************************************************************ELGCSIHS
00091                                                                   ELGCSIHS
00092  DATA DIVISION.                                                   ELGCSIHS
00093  WORKING-STORAGE SECTION.                                         ELGCSIHS
00094  01  WS-MISC.                                                     ELGCSIHS
00095      05  FILLER                  PIC  X(24) VALUE                 ELGCSIHS
00096          '** ELGCSIHS WS BEGINS **'.                              ELGCSIHS
00097                                                                   ELGCSIHS
00098  01  SWITCHES.                                                    ELGCSIHS
00099      05  WS-BASIC-DAYS-SW        PIC  X(01) VALUE 'B'.            ELGCSIHS
00100          88  BASIC-NOT-FOUND                VALUE 'B'.            ELGCSIHS
00101          88  BASIC-FOUND                    VALUE 'F'.            ELGCSIHS
00102          88  BASIC-UNLIMITED-FOUND          VALUE 'U'.            ELGCSIHS
00103          88  BASIC-PROCESS-FINISHED         VALUE 'F' 'U'.        ELGCSIHS
00104                                                                   ELGCSIHS
00105      05  WS-IP-FIELD-SW          PIC  X(01) VALUE SPACE.          ELGCSIHS
00106          88  VALID-IP-FIELD                 VALUE 'V'.            ELGCSIHS
00107          88  INVALID-IP-FIELD               VALUE 'I'.            ELGCSIHS
00108                                                                   ELGCSIHS
00109      05  WS-ALCOHOL-SW           PIC  X(01) VALUE SPACE.          ELGCSIHS
00110          88  ALCOHOL-NOT-FOUND              VALUE SPACE.          ELGCSIHS
00111          88  ALCOHOL-FOUND                  VALUE '1'.            ELGCSIHS
00112                                                                   ELGCSIHS
00113      05  WS-DRUG-SW              PIC  X(01) VALUE SPACE.          ELGCSIHS
00114          88  DRUG-NOT-FOUND                 VALUE SPACE.          ELGCSIHS
00115          88  DRUG-FOUND                     VALUE '2'.            ELGCSIHS
00116                                                                   ELGCSIHS
00117      05  WS-MENTAL-SW            PIC  X(01) VALUE SPACE.          ELGCSIHS
00118          88  MENTAL-NOT-FOUND               VALUE SPACE.          ELGCSIHS
00119          88  MENTAL-FOUND                   VALUE '3'.            ELGCSIHS
00120                                                                   ELGCSIHS
00121      05  WS-PROVISION-SW         PIC  X(01) VALUE SPACE.          ELGCSIHS
00122          88  PROVISION-FOUND                VALUE 'Y'.            ELGCSIHS
00123                                                                   ELGCSIHS
00124  01  PROGRAM-CONSTANTS.                                           ELGCSIHS
00125      05  PC-ALC                  PIC X(07) VALUE 'ALCOHOL'.       ELGCSIHS
00126      05  PC-ABM                  PIC X(06) VALUE '#ABM  '.        ELGCSIHS
00127      05  PC-ANC                  PIC X(06) VALUE '@ANC  '.        ELGCSIHS
00128      05  PC-CHC-W                PIC X(06) VALUE 'CHC  W'.        ELGCSIHS
00129      05  PC-COMBINED             PIC X(08) VALUE 'COMBINED'.      ELGCSIHS
00130      05  PC-DRUG                 PIC X(04) VALUE 'DRUG'.          ELGCSIHS
00131      05  PC-DRUG-ALC             PIC X(12) VALUE 'DRUG/ALCOHOL'.  ELGCSIHS
00132      05  PC-EACH                 PIC X(04) VALUE 'EACH'.          ELGCSIHS
00133      05  PC-EXCLUDING            PIC X(10) VALUE 'EXCLUDING '.    ELGCSIHS
00134      05  PC-FAM-REL-LVL-MEDIC    PIC X(02) VALUE '0M'.            ELGCSIHS
00135      05  PC-IBGR                 PIC X(06) VALUE '#IBGR '.        ELGCSIHS
00136      05  PC-LEFT-SIDE-BAR-ONLY   PIC X(33) VALUE                  ELGCSIHS
00137          '                              |  '.                     ELGCSIHS
00138      05  PC-MAXIMUM-NBR-OCCURS   PIC 9(02) VALUE 29.              ELGCSIHS
00139      05  PC-MENT                 PIC X(06) VALUE 'MENTAL'.        ELGCSIHS
00140      05  PC-MENT-ALC             PIC X(14) VALUE 'MENTAL/ALCOHOL'.ELGCSIHS
00141      05  PC-MENT-DRUG            PIC X(11) VALUE 'MENTAL/DRUG'.   ELGCSIHS
00142      05  PC-MENT-DRUG-ALC        PIC X(19) VALUE 'MENTAL/DRUG/ALCOELGCSIHS
00143 -        'HOL'.                                                   ELGCSIHS
00144      05  PC-PER                  PIC X(04) VALUE 'PER '.          ELGCSIHS
00145      05  PC-UNLIMITED            PIC X(09) VALUE 'UNLIMITED'.     ELGCSIHS
00146                                                                   ELGCSIHS
00147                                                                   ELGCSIHS
00148  01  WS-COMBO-CNT                PIC S9(02) VALUE +0  COMP-3.     ELGCSIHS
00149  01  WS-TRUE-DAY-CNT             PIC S9(02) VALUE +0  COMP-3.     ELGCSIHS
00150  01  WS-HOLD-GROUP-ABM-SLOT      PIC S9(07) VALUE +0  COMP-3.     ELGCSIHS
00151  01  WS-HOLD-CONTRACT-ABM-SLOT   PIC S9(07) VALUE +0  COMP-3.     ELGCSIHS
00152  01  WS-PROVISION-SLOT-NO        PIC S9(07) VALUE +0  COMP-3.     ELGCSIHS
00153  01  WS-ATBL-X-SUB               PIC S9(4) COMP.                  ELGCSIHS
00154                                                                   ELGCSIHS
00155  01  WS-ABM-OCCURENCE.                                            ELGCSIHS
00156      05  WS-PLACE-OF-TREATMENT   PIC X(02).                       ELGCSIHS
00157          88  INPATIENT-POT                  VALUE '0B' '0C' '0D'  ELGCSIHS
00158                                              '0R' '0S' '01' '02'  ELGCSIHS
00159                                              '03' '05' '06'.      ELGCSIHS
00160                                                                   ELGCSIHS
00161      05  WS-CONDITION-BITS.                                       ELGCSIHS
00162 **** THIS WILL EXPLAIN WHAT EACH VALUE INDICATES ****             ELGCSIHS
00163 **** FOR THE FIRST THREE CONDITION BITS          ****             ELGCSIHS
00164 **** ALL CONDITION                    '100'      ****             ELGCSIHS
00165 **** ALL CONDITION EXCEPT             '110'      ****             ELGCSIHS
00166 **** ALL DIAGNOSIS CONDITION          '001'      ****             ELGCSIHS
00167 **** ALL DIAGNOSIS CONDITION EXCEPT   '011'      ****             ELGCSIHS
00168 **** PERTAINS TO INDIVIDUAL BITS      '000'      ****             ELGCSIHS
00169          10  FILLER              PIC X(03).                       ELGCSIHS
00170              88  BITS-NO-EXCEPT             VALUE '100' '001'.    ELGCSIHS
00171              88  BITS-WITH-EXCEPT           VALUE '110' '011'.    ELGCSIHS
00172              88  BITS-FOR-INDIVIDUAL        VALUE '000'.          ELGCSIHS
00173          10  WS-BIT-1            PIC X(01).                       ELGCSIHS
00174              88  TUBERCULOSIS               VALUE '1'.            ELGCSIHS
00175          10  WS-BIT-2            PIC X(01).                       ELGCSIHS
00176              88  MENTAL                     VALUE '1'.            ELGCSIHS
00177          10  WS-BIT-3            PIC X(01).                       ELGCSIHS
00178              88  DRUG                       VALUE '1'.            ELGCSIHS
00179          10  WS-BIT-4            PIC X(01).                       ELGCSIHS
00180              88  ALCOHOL                    VALUE '1'.            ELGCSIHS
00181 **** THE FILLER BELOW CONTAINS THE REST OF CONDITION BITS FROM    ELGCSIHS
00182 **** RECORD BUT ARE NOT NEEDED FOR ANY COMPARISIONS.              ELGCSIHS
00183          10  FILLER              PIC X(13).                       ELGCSIHS
00184                                                                   ELGCSIHS
00185  01  WS-HOLD-AREA.                                                ELGCSIHS
00186      05  WS-BASIC-VALUE          PIC 9(07)V99  VALUE ZERO.        ELGCSIHS
00187      05  WS-UNLIMITED-CHECK REDEFINES WS-BASIC-VALUE              ELGCSIHS
00188                                  PIC X(09).                       ELGCSIHS
00189          88  BASIC-UNLIMITED                   VALUE              ELGCSIHS
00190              '999999999' '999999900'.                             ELGCSIHS
00191      05  WS-ECF-VALUE            PIC 9(05)V99  VALUE ZERO.        ELGCSIHS
00192      05  WS-CHC-VALUE            PIC 9(05)V99  VALUE ZERO.        ELGCSIHS
00193                                                                   ELGCSIHS
00194      05  WS-HOLD-LINE.                                            ELGCSIHS
00195          10  WS-TITLE            PIC X(33).                       ELGCSIHS
00196          10  WS-SENTENCE         PIC X(46).                       ELGCSIHS
00197                                                                   ELGCSIHS
00198      05  WS-ANC-PAYMENT-LVL      PIC S9(04)    VALUE +0 COMP.     ELGCSIHS
00199                                                                   ELGCSIHS
00200      05  WS-PROVISION-ID-KEY     PIC X(06)     VALUE SPACES.      ELGCSIHS
00201          88  NON-MEDICARE-DRB                  VALUE 'DRB  A'.    ELGCSIHS
00202          88  MEDICARE-MCOI                     VALUE 'MCOI A'.    ELGCSIHS
00203                                                                   ELGCSIHS
00204 ***************************************************************   ELGCSIHS
00205 ** THESE FIELDS ARE FOR THE BENEFIT PERIOD AND INTERVAL LINES     ELGCSIHS
00206 ***************************************************************   ELGCSIHS
00207      05  WS-BENEFIT-PERIOD       PIC X(02).                       ELGCSIHS
00208      05  WS-INTERVAL-TIME-FACTOR PIC  ZZ9.                        ELGCSIHS
00209      05  WS-INTERVAL-TYPE        PIC X(02).                       ELGCSIHS
00210                                                                   ELGCSIHS
00211 ***************************************************************   ELGCSIHS
00212 ** THESE FIELDS ARE USED FOR COMPUTING ECF AND CHC DAYS           ELGCSIHS
00213 ** NOTE : THE '-DRR-' STANDS FOR DAYS REDUCTION RATE              ELGCSIHS
00214 ***************************************************************   ELGCSIHS
00215      05  WS-GPA-DRR-IND          PIC X(01).                       ELGCSIHS
00216      05  WS-GPA-DRR-BASIC-APL    PIC S99V9  VALUE +0   COMP-3.    ELGCSIHS
00217      05  WS-GPA-DRR-BASIC-BASE   PIC S99V9  VALUE +0   COMP-3.    ELGCSIHS
00218      05  WS-GPW-DRR-IND          PIC X(01).                       ELGCSIHS
00219      05  WS-GPW-DRR-BASIC-APL    PIC S99V9  VALUE +0   COMP-3.    ELGCSIHS
00220      05  WS-GPW-DRR-BASIC-BASE   PIC S99V9  VALUE +0   COMP-3.    ELGCSIHS
00221                                                                   ELGCSIHS
00222 ***************************************************************   ELGCSIHS
00223 ** THESE ARE 'TRUE DAYS' WHICH WILL NEEDED TO COMPARE AGAINST     ELGCSIHS
00224 ** ONE ANOTHER TO COMBINE IF THEY MATCH.                          ELGCSIHS
00225 ***************************************************************   ELGCSIHS
00226      05  WS-ALCOHOL-DAYS         PIC S9(07)V99 VALUE +0 COMP-3.   ELGCSIHS
00227      05  WS-DRUG-DAYS            PIC S9(07)V99 VALUE +0 COMP-3.   ELGCSIHS
00228      05  WS-MENTAL-DAYS          PIC S9(07)V99 VALUE +0 COMP-3.   ELGCSIHS
00229                                                                   ELGCSIHS
00230  01  WS-FIXED-TEXT-DESCRIPTION.                                   ELGCSIHS
00231      05  WS-IHS-HEADER-1.                                         ELGCSIHS
00232          10  FILLER              PIC X(70) VALUE 'THE FOLLOWING DIELGCSIHS
00233 -        'SPLAYED BENEFITS ARE FOR: INPATIENT HOSPITAL SERVICES'. ELGCSIHS
00234      05  WS-DASH-LINE.                                            ELGCSIHS
00235          10  FILLER              PIC X(70) VALUE '----------------ELGCSIHS
00236 -        '------------------------------------------------------'.ELGCSIHS
00237          10  FILLER              PIC X(09) VALUE '---------'.     ELGCSIHS
00238                                                                   ELGCSIHS
00239 **************************************************************    ELGCSIHS
00240 ***** THIS TEXT WILL BE THE SUBJECT OF THE INFORMATION GIVEN      ELGCSIHS
00241 ***** IT WILL BE DISPLAYED ON THE LEFT HAND SIDE OF SCREEN        ELGCSIHS
00242 **************************************************************    ELGCSIHS
00243      05  WS-IP-DAYS-PHRASE.                                       ELGCSIHS
00244          10  FILLER              PIC X(04) VALUE SPACES.          ELGCSIHS
00245          10  FILLER              PIC X(29) VALUE ' NUMBER OF INPATELGCSIHS
00246 -            'IENT DAYS |  '.                                     ELGCSIHS
00247      05  WS-BP-PHRASE.                                            ELGCSIHS
00248          10  FILLER              PIC X(05) VALUE SPACES.          ELGCSIHS
00249          10  FILLER              PIC X(09) VALUE 'BASIC DAY'.     ELGCSIHS
00250          10  FILLER              PIC X(19) VALUE ' BENEFIT PERIOD ELGCSIHS
00251 -            '|  '.                                               ELGCSIHS
00252      05  WS-INTERVAL-PHRASE.                                      ELGCSIHS
00253          10  FILLER              PIC X(11) VALUE SPACES.          ELGCSIHS
00254          10  FILLER              PIC X(09) VALUE 'BASIC DAY'.     ELGCSIHS
00255          10  FILLER              PIC X(13) VALUE ' INTERVAL |  '. ELGCSIHS
00256                                                                   ELGCSIHS
00257  01  WS-MESSAGE-AREA.                                             ELGCSIHS
00258      05  WS-CODING-ERROR.                                         ELGCSIHS
00259          10  FILLER              PIC X(30) VALUE SPACES.          ELGCSIHS
00260          10  FILLER              PIC X(03) VALUE '|  '.           ELGCSIHS
00261          10  FILLER              PIC X(46) VALUE '** A CONTRACT COELGCSIHS
00262 -            'DING ERROR OCCURRED FOR **'.                        ELGCSIHS
00263      05  WS-DRB-NOT-COVERED.                                      ELGCSIHS
00264          10  FILLER              PIC X(32) VALUE 'DAILY ROOM AND BELGCSIHS
00265 -        'OARD NOT COVERED'.                                      ELGCSIHS
00266                                                                   ELGCSIHS
00267  01  WS-FIRST-LINE-TEXT.                                          ELGCSIHS
00268      05  WS-BASIC-PHRASE.                                         ELGCSIHS
00269          10  FILLER              PIC X(06)     VALUE 'BASIC-'.    ELGCSIHS
00270          10  WS-BASIC-DAYS       PIC Z(07)V99.                    ELGCSIHS
00271                                                                   ELGCSIHS
00272      05  WS-ECF-PHRASE.                                           ELGCSIHS
00273          10  FILLER              PIC X(07)     VALUE ' | ECF-'.   ELGCSIHS
00274          10  WS-ECF-DAYS         PIC Z(05)V99.                    ELGCSIHS
00275                                                                   ELGCSIHS
00276      05  WS-CHC-PHRASE.                                           ELGCSIHS
00277          10  FILLER              PIC X(07)     VALUE ' | CHC-'.   ELGCSIHS
00278          10  WS-CHC-DAYS         PIC Z(05)V99.                    ELGCSIHS
00279                                                                   ELGCSIHS
00280  01  WS-AREA-FOR-DAYS.                                            ELGCSIHS
00281      05  WS-INFO                 PIC X(19) VALUE SPACES.          ELGCSIHS
00282      05  FILLER                  PIC X(01) VALUE '-'.             ELGCSIHS
00283      05  WS-DAYS-VALUE           PIC Z(07)V99.                    ELGCSIHS
00284      05  WS-DAYS-UNLIMITED-CK    REDEFINES WS-DAYS-VALUE          ELGCSIHS
00285                                  PIC X(09).                       ELGCSIHS
00286          88  UNLIMITED-AMT                 VALUE                  ELGCSIHS
00287              '999999999' '999999900'.                             ELGCSIHS
00288      05  FILLER                  PIC X(01) VALUE SPACE.           ELGCSIHS
00289      05  WS-INDICATOR            PIC X(08) VALUE SPACES.          ELGCSIHS
00290      05  FILLER                  PIC X(07) VALUE SPACES.          ELGCSIHS
00291                                                                   ELGCSIHS
00292  LINKAGE SECTION.                                                 ELGCSIHS
00293  01  DFHCOMMAREA.                                                 ELGCSIHS
00294      COPY ELSCOMMC.                                               ELGCSIHS
00295 /                                                                 ELGCSIHS
00296      COPY ELSCIA2C.                                               ELGCSIHS
00297 /                                                                 ELGCSIHS
00298      COPY ELSIOPMC.                                               ELGCSIHS
00299 /                                                                 ELGCSIHS
00300      COPY ELSKEYSC.                                               ELGCSIHS
00301 /                                                                 ELGCSIHS
00302      COPY ELSSSCBC.                                               ELGCSIHS
00303 /                                                                 ELGCSIHS
00304      COPY ELSTCWAC.                                               ELGCSIHS
00305 /                                                                 ELGCSIHS
00306      COPY ELSOUTPC.                                               ELGCSIHS
00307 /    COPYBOOK FOR CONTRACT SUMMARY POINTER TABLE                  ELGCSIHS
00308      COPY ELSCSPTC.                                               ELGCSIHS
00309 /    COPYBOOK FOR CONTRACT SUMMARY BENEFIT PROVISION TABLE        ELGCSIHS
00310      COPY ELSCSBPC.                                               ELGCSIHS
00311 /    COPYBOOK FOR CONTRACT SUMMARY ACCUMULATOR TABLE POINTERS     ELGCSIHS
00312      COPY ELSCSACC.                                               ELGCSIHS
00313 /    COPYBOOK FOR CONTRACT SUMMARY ACCUMULATOR TABLE              ELGCSIHS
00314      COPY ELSATBLC.                                               ELGCSIHS
00315 /    CODES MANUAL DESCRIPTION AREA                                ELGCSIHS
00316      COPY ELSCMDSC.                                               ELGCSIHS
00317 /    CODES MANUAL INTERFACE AREA                                  ELGCSIHS
00318      COPY ELSCMIFC.                                               ELGCSIHS
00319 /                                                                 ELGCSIHS
00320  01  BENEFIT-PROVISION-RECORD.                                    ELGCSIHS
00321      COPY GCBENPVC.                                               ELGCSIHS
00322 /                                                                 ELGCSIHS
00323  01  CONTRACT-RECORD.                                             ELGCSIHS
00324      COPY GCCONTRC.                                               ELGCSIHS
00325 /                                                                 ELGCSIHS
00326  01  GROUP-SPECIFIC-RECORD.                                       ELGCSIHS
00327      COPY GCGROUPC.                                               ELGCSIHS
00328 /                                                                 ELGCSIHS
00329  01  IBGR-RECORD.                                                 ELGCSIHS
00330      COPY GCTIBGRC.                                               ELGCSIHS
00331      EJECT                                                        ELGCSIHS
00332  PROCEDURE DIVISION.                                              ELGCSIHS
00333 ************************************************************      ELGCSIHS
00334 *                                                          *      ELGCSIHS
00335 *                    PROCEDURE DIVISION                    *      ELGCSIHS
00336 *                                                          *      ELGCSIHS
00337 ************************************************************      ELGCSIHS
00338                                                                   ELGCSIHS
00339                                                                   ELGCSIHS
00340 ************************************************************      ELGCSIHS
00341 *                                                          *      ELGCSIHS
00342 *        DO ELGCSIHS                                       *      ELGCSIHS
00343 *                                                          *      ELGCSIHS
00344 ************************************************************      ELGCSIHS
00345  DO-ELGCSIHS.                                                     ELGCSIHS
00346      PERFORM INITIALIZATION.                                      ELGCSIHS
00347      PERFORM PROCESS.                                             ELGCSIHS
00348      GOBACK.                                                      ELGCSIHS
00349                                                                   ELGCSIHS
00350                                                                   ELGCSIHS
00351 ************************************************************      ELGCSIHS
00352 *                                                          *      ELGCSIHS
00353 *        INITIALIZATION.                                   *      ELGCSIHS
00354 *                                                          *      ELGCSIHS
00355 ************************************************************      ELGCSIHS
00356  INITIALIZATION.                                                  ELGCSIHS
00357      PERFORM ESTABLISH-ADDRESS-OF-CO.                             ELGCSIHS
00358      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELGCSIHS
00359      PERFORM OBTAIN-ABM-SLOT-NUMBERS.                             ELGCSIHS
00360                                                                   ELGCSIHS
00361                                                                   ELGCSIHS
00362 ************************************************************      ELGCSIHS
00363 *                                                          *      ELGCSIHS
00364 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGCSIHS
00365 *                                                          *      ELGCSIHS
00366 ************************************************************      ELGCSIHS
00367  ESTABLISH-ADDRESS-OF-CO.                                         ELGCSIHS
00368      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELGCSIHS
00369      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELGCSIHS
00370      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELGCSIHS
00371                                                                   ELGCSIHS
00372                                                                   ELGCSIHS
00373 ************************************************************      ELGCSIHS
00374 *                                                          *      ELGCSIHS
00375 *        CHECK FOR VALID COMMAREA                          *      ELGCSIHS
00376 *                                                          *      ELGCSIHS
00377 ************************************************************      ELGCSIHS
00378  CHECK-FOR-VALID-COMMAREA.                                        ELGCSIHS
00379      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCSIHS
00380         EXEC CICS ABEND                                           ELGCSIHS
00381                   ABCODE('EL01')                                  ELGCSIHS
00382            END-EXEC.                                              ELGCSIHS
00383                                                                   ELGCSIHS
00384 ************************************************************      ELGCSIHS
00385 *                                                          *      ELGCSIHS
00386 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELGCSIHS
00387 *                                                          *      ELGCSIHS
00388 ************************************************************      ELGCSIHS
00389  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELGCSIHS
00390      IF ECA-CIA-PTR = NULL                                        ELGCSIHS
00391          PERFORM SIGNAL-INVALID-CIA.                              ELGCSIHS
00392      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGCSIHS
00393          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGCSIHS
00394                                                                   ELGCSIHS
00395                                                                   ELGCSIHS
00396 ************************************************************      ELGCSIHS
00397 *                                                          *      ELGCSIHS
00398 *        SIGNAL INVALID CIA                                *      ELGCSIHS
00399 *                                                          *      ELGCSIHS
00400 ************************************************************      ELGCSIHS
00401  SIGNAL-INVALID-CIA.                                              ELGCSIHS
00402      EXEC CICS ABEND                                              ELGCSIHS
00403                ABCODE('EL02')                                     ELGCSIHS
00404         END-EXEC.                                                 ELGCSIHS
00405                                                                   ELGCSIHS
00406 ************************************************************      ELGCSIHS
00407 *                                                          *      ELGCSIHS
00408 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELGCSIHS
00409 *                                                          *      ELGCSIHS
00410 ************************************************************      ELGCSIHS
00411  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELGCSIHS
00412      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGCSIHS
00413      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00414          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELGCSIHS
00415      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00416          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIHS
00417                                                                   ELGCSIHS
00418 ************************************************************      ELGCSIHS
00419 *                                                          *      ELGCSIHS
00420 *        SIGNAL UNALLOC AREA ERROR                         *      ELGCSIHS
00421 *                                                          *      ELGCSIHS
00422 ************************************************************      ELGCSIHS
00423  SIGNAL-UNALLOC-AREA-ERROR.                                       ELGCSIHS
00424      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCSIHS
00425      PERFORM SIGNAL-ABEND.                                        ELGCSIHS
00426                                                                   ELGCSIHS
00427 ************************************************************      ELGCSIHS
00428 *                                                          *      ELGCSIHS
00429 *        SIGNAL ABEND                                      *      ELGCSIHS
00430 *                                                          *      ELGCSIHS
00431 ************************************************************      ELGCSIHS
00432  SIGNAL-ABEND.                                                    ELGCSIHS
00433      EXEC CICS ABEND                                              ELGCSIHS
00434                ABCODE(CIA-ABCODE)                                 ELGCSIHS
00435         END-EXEC.                                                 ELGCSIHS
00436                                                                   ELGCSIHS
00437 ************************************************************      ELGCSIHS
00438 *                                                          *      ELGCSIHS
00439 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELGCSIHS
00440 *                                                          *      ELGCSIHS
00441 ************************************************************      ELGCSIHS
00442  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELGCSIHS
00443      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELGCSIHS
00444      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELGCSIHS
00445      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELGCSIHS
00446      PERFORM ESTABLISH-ADDRESSABILITY-CODES.                      ELGCSIHS
00447      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELGCSIHS
00448      PERFORM ESTABLISH-ADDRESSABILITY-OF-CR.                      ELGCSIHS
00449      PERFORM ESTABLISH-ADDRESSABILITY-CSAC.                       ELGCSIHS
00450      PERFORM ESTABLISH-ADDRESSABILITY-CSPT.                       ELGCSIHS
00451      PERFORM ESTABLISH-ADDRESSABILITY-CSBP.                       ELGCSIHS
00452                                                                   ELGCSIHS
00453 ************************************************************      ELGCSIHS
00454 *                                                          *      ELGCSIHS
00455 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELGCSIHS
00456 *                                                          *      ELGCSIHS
00457 ************************************************************      ELGCSIHS
00458  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELGCSIHS
00459      SET  CIA-ELSKEYS-DDN TO  TRUE.                               ELGCSIHS
00460      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00461          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGCSIHS
00462      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00463          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIHS
00464                                                                   ELGCSIHS
00465 ************************************************************      ELGCSIHS
00466 *                                                          *      ELGCSIHS
00467 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION AREA *      ELGCSIHS
00468 *                                                          *      ELGCSIHS
00469 ************************************************************      ELGCSIHS
00470  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELGCSIHS
00471      SET  CIA-ELSTCWA-DDN TO TRUE.                                ELGCSIHS
00472      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00473          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGCSIHS
00474      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00475          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIHS
00476                                                                   ELGCSIHS
00477 ************************************************************      ELGCSIHS
00478 *                                                          *      ELGCSIHS
00479 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGCSIHS
00480 *                                                          *      ELGCSIHS
00481 ************************************************************      ELGCSIHS
00482  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELGCSIHS
00483      SET  CIA-ELSOUTP-DDN TO TRUE.                                ELGCSIHS
00484      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00485          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGCSIHS
00486      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00487          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIHS
00488                                                                   ELGCSIHS
00489 ************************************************************      ELGCSIHS
00490 *                                                          *      ELGCSIHS
00491 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL TRANSLATI*      ELGCSIHS
00492 *                                                          *      ELGCSIHS
00493 ************************************************************      ELGCSIHS
00494  ESTABLISH-ADDRESSABILITY-CODES.                                  ELGCSIHS
00495      SET  CIA-ELSCMIF-DDN TO TRUE.                                ELGCSIHS
00496      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00497          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGCSIHS
00498      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00499          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIHS
00500                                                                   ELGCSIHS
00501 ************************************************************      ELGCSIHS
00502 *                                                          *      ELGCSIHS
00503 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC RECORD *      ELGCSIHS
00504 *                                                          *      ELGCSIHS
00505 ************************************************************      ELGCSIHS
00506  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELGCSIHS
00507      SET  CIA-ELSGRPSP-DDN TO TRUE.                               ELGCSIHS
00508      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00509          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELGCSIHS
00510      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00511          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIHS
00512                                                                   ELGCSIHS
00513 ************************************************************      ELGCSIHS
00514 *                                                          *      ELGCSIHS
00515 *        ESTABLISH ADDRESSABILITY OF CONTRACT RECORD       *      ELGCSIHS
00516 *                                                          *      ELGCSIHS
00517 ************************************************************      ELGCSIHS
00518  ESTABLISH-ADDRESSABILITY-OF-CR.                                  ELGCSIHS
00519      SET  CIA-ELSCONIB-DDN TO TRUE.                               ELGCSIHS
00520      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00521          ADDRESS OF CONTRACT-RECORD.                              ELGCSIHS
00522                                                                   ELGCSIHS
00523 ************************************************************      ELGCSIHS
00524 *                                                          *      ELGCSIHS
00525 *        ESTABLISH ADDRESSABILITY OF CS ACCUMULATOR TABLE  *      ELGCSIHS
00526 *                                                          *      ELGCSIHS
00527 ************************************************************      ELGCSIHS
00528  ESTABLISH-ADDRESSABILITY-CSAC.                                   ELGCSIHS
00529      SET  CIA-ELSCSAC-DDN TO TRUE.                                ELGCSIHS
00530      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00531          ADDRESS OF CSAC-ACCUMULATOR-TABLE.                       ELGCSIHS
00532      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00533          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIHS
00534                                                                   ELGCSIHS
00535 ************************************************************      ELGCSIHS
00536 *                                                          *      ELGCSIHS
00537 *        ESTABLISH ADDRESSABILITY OF CS POINTER TABLE      *      ELGCSIHS
00538 *                                                          *      ELGCSIHS
00539 ************************************************************      ELGCSIHS
00540  ESTABLISH-ADDRESSABILITY-CSPT.                                   ELGCSIHS
00541      SET  CIA-ELSCSPTC-DDN TO TRUE.                               ELGCSIHS
00542      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00543          ADDRESS OF CSPT-POINTER-LIST.                            ELGCSIHS
00544      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00545          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSIHS
00546                                                                   ELGCSIHS
00547 ************************************************************      ELGCSIHS
00548 *                                                          *      ELGCSIHS
00549 *        ESTABLISH ADDRESSABILITY OF CS BENEFIT PROVISION T*      ELGCSIHS
00550 *                                                          *      ELGCSIHS
00551 ************************************************************      ELGCSIHS
00552  ESTABLISH-ADDRESSABILITY-CSBP.                                   ELGCSIHS
00553      IF CSPT-IHS-BP-TBL-PTR = NULL                                ELGCSIHS
00554          PERFORM SIGNAL-UNALLOC-AREA-ERROR                        ELGCSIHS
00555      ELSE                                                         ELGCSIHS
00556          PERFORM ESTABLISH-ADDRESS-OF-CSBPC.                      ELGCSIHS
00557                                                                   ELGCSIHS
00558 ************************************************************      ELGCSIHS
00559 *                                                          *      ELGCSIHS
00560 *        ESTABLISH ADDRESS OF CSBPC                        *      ELGCSIHS
00561 *                                                          *      ELGCSIHS
00562 ************************************************************      ELGCSIHS
00563  ESTABLISH-ADDRESS-OF-CSBPC.                                      ELGCSIHS
00564      SET ADDRESS OF CSBP-BENEFIT-PROVISION-TABLE TO               ELGCSIHS
00565          CSPT-IHS-BP-TBL-PTR.                                     ELGCSIHS
00566                                                                   ELGCSIHS
00567 ************************************************************      ELGCSIHS
00568 *                                                          *      ELGCSIHS
00569 *        OBTAIN ABM SLOT NUMBERS                           *      ELGCSIHS
00570 *                                                          *      ELGCSIHS
00571 ************************************************************      ELGCSIHS
00572  OBTAIN-ABM-SLOT-NUMBERS.                                         ELGCSIHS
00573      PERFORM SEARCH-GROUP-FOR-ABM                                 ELGCSIHS
00574          VARYING GCG-INDEX FROM 1 BY 1                            ELGCSIHS
00575                     UNTIL GCG-INDEX >                             ELGCSIHS
00576              GCG-COUNT-TAB-PROVN-POINTERS                         ELGCSIHS
00577                        OR WS-HOLD-GROUP-ABM-SLOT > ZERO.          ELGCSIHS
00578      IF ADDRESS OF CONTRACT-RECORD NOT = NULL                     ELGCSIHS
00579          PERFORM OBTAIN-CONTRACT-SLOT-NUMBER.                     ELGCSIHS
00580                                                                   ELGCSIHS
00581                                                                   ELGCSIHS
00582 ************************************************************      ELGCSIHS
00583 *                                                          *      ELGCSIHS
00584 *        OBTAIN CONTRACT SLOT NUMBER                       *      ELGCSIHS
00585 *                                                          *      ELGCSIHS
00586 ************************************************************      ELGCSIHS
00587  OBTAIN-CONTRACT-SLOT-NUMBER.                                     ELGCSIHS
00588      MOVE GCT-TAB2-SLOT-NO TO                                     ELGCSIHS
00589          WS-HOLD-CONTRACT-ABM-SLOT.                               ELGCSIHS
00590                                                                   ELGCSIHS
00591                                                                   ELGCSIHS
00592 ************************************************************      ELGCSIHS
00593 *                                                          *      ELGCSIHS
00594 *        SEARCH GROUP FOR ABM                              *      ELGCSIHS
00595 *                                                          *      ELGCSIHS
00596 ************************************************************      ELGCSIHS
00597  SEARCH-GROUP-FOR-ABM.                                            ELGCSIHS
00598      IF GCG-TAB-ID (GCG-INDEX) = PC-ABM                           ELGCSIHS
00599           MOVE GCG-TAB-SLOT-NO (GCG-INDEX) TO                     ELGCSIHS
00600                WS-HOLD-GROUP-ABM-SLOT                             ELGCSIHS
00601        END-IF.                                                    ELGCSIHS
00602                                                                   ELGCSIHS
00603 ************************************************************      ELGCSIHS
00604 *                                                          *      ELGCSIHS
00605 *        PROCESS                                           *      ELGCSIHS
00606 *                                                          *      ELGCSIHS
00607 ************************************************************      ELGCSIHS
00608  PROCESS.                                                         ELGCSIHS
00609      PERFORM INITIALIZE-TEXT-COMPRESSION-AR.                      ELGCSIHS
00610      PERFORM DISPLAY-HEADINGS-FOR-SUBTOPIC.                       ELGCSIHS
00611      IF GCG-FAM-REL-LVL = PC-FAM-REL-LVL-MEDIC                    ELGCSIHS
00612          PERFORM CHECK-MEDICARE-DAILY-ROOM-ANDX                   ELGCSIHS
00613      ELSE IF ADDRESS OF CONTRACT-RECORD NOT = NULL                ELGCSIHS
00614          PERFORM CHECK-CONTRACT-MEDICARE-DAILYX                   ELGCSIHS
00615      ELSE                                                         ELGCSIHS
00616          PERFORM CHECK-NON-MEDICARE-DAILY-ROOMX.                  ELGCSIHS
00617      PERFORM PROCESS-ALL-OTHER-DAYS.                              ELGCSIHS
00618      IF BASIC-FOUND                                               ELGCSIHS
00619          PERFORM CREATE-BENEFIT-AND-INTERVAL-LI.                  ELGCSIHS
00620      SET PROCESS-IHS  TO TRUE.                                    ELGCSIHS
00621      PERFORM CALL-SENTENCE-INTERFACE.                             ELGCSIHS
00622      PERFORM SEARCH-FOR-DUMMY-ANCILLARY-PRO.                      ELGCSIHS
00623      PERFORM CALL-CONTRACT-SUMMARY-OUTPUT-I.                      ELGCSIHS
00624      SET COMPLETED-IHS TO TRUE.                                   ELGCSIHS
00625                                                                   ELGCSIHS
00626 ************************************************************      ELGCSIHS
00627 *                                                          *      ELGCSIHS
00628 *        CHECK CONTRACT MEDICARE DAILY ROOM AND BOARD PROVI*      ELGCSIHS
00629 *                                                          *      ELGCSIHS
00630 ************************************************************      ELGCSIHS
00631  CHECK-CONTRACT-MEDICARE-DAILYX.                                  ELGCSIHS
00632      IF GCT-FAM-REL-LVL = PC-FAM-REL-LVL-MEDIC                    ELGCSIHS
00633          PERFORM CHECK-MEDICARE-DAILY-ROOM-ANDX                   ELGCSIHS
00634      ELSE                                                         ELGCSIHS
00635          PERFORM CHECK-NON-MEDICARE-DAILY-ROOMX.                  ELGCSIHS
00636                                                                   ELGCSIHS
00637 ************************************************************      ELGCSIHS
00638 *                                                          *      ELGCSIHS
00639 *        DISPLAY HEADINGS FOR SUBTOPIC                     *      ELGCSIHS
00640 *                                                          *      ELGCSIHS
00641 ************************************************************      ELGCSIHS
00642  DISPLAY-HEADINGS-FOR-SUBTOPIC.                                   ELGCSIHS
00643      SET COF-NEW-PAGE     TO TRUE.                                ELGCSIHS
00644      MOVE +3              TO COF-NBR-HDR-LINES.                   ELGCSIHS
00645      MOVE +0              TO COF-NBR-DTL-LINES.                   ELGCSIHS
00646      MOVE WS-IHS-HEADER-1 TO COF-HDR-LINE (2).                    ELGCSIHS
00647      MOVE WS-DASH-LINE    TO COF-HDR-LINE (3).                    ELGCSIHS
00648      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSIHS
00649                                                                   ELGCSIHS
00650 ************************************************************      ELGCSIHS
00651 *                                                          *      ELGCSIHS
00652 *        CHECK MEDICARE DAILY ROOM AND BOARD PROVISION     *      ELGCSIHS
00653 *                                                          *      ELGCSIHS
00654 ************************************************************      ELGCSIHS
00655  CHECK-MEDICARE-DAILY-ROOM-ANDX.                                  ELGCSIHS
00656      SET MEDICARE-MCOI       TO TRUE.                             ELGCSIHS
00657      PERFORM FIND-OCCURENCE-OF-DAILY-ROOM-A                       ELGCSIHS
00658          VARYING CSBP-X-IDX FROM +1 BY +1                         ELGCSIHS
00659                 UNTIL   CSBP-X-IDX > CSBP-TBL-CNT                 ELGCSIHS
00660                 OR      PROVISION-FOUND.                          ELGCSIHS
00661                                                                   ELGCSIHS
00662 ************************************************************      ELGCSIHS
00663 *                                                          *      ELGCSIHS
00664 *        CHECK NON-MEDICARE DAILY ROOM AND BOARD PROVISION *      ELGCSIHS
00665 *                                                          *      ELGCSIHS
00666 ************************************************************      ELGCSIHS
00667  CHECK-NON-MEDICARE-DAILY-ROOMX.                                  ELGCSIHS
00668      SET NON-MEDICARE-DRB    TO TRUE.                             ELGCSIHS
00669      PERFORM FIND-OCCURENCE-OF-DAILY-ROOM-A                       ELGCSIHS
00670          VARYING CSBP-X-IDX FROM +1 BY +1                         ELGCSIHS
00671                 UNTIL   CSBP-X-IDX > CSBP-TBL-CNT                 ELGCSIHS
00672                 OR      PROVISION-FOUND.                          ELGCSIHS
00673                                                                   ELGCSIHS
00674                                                                   ELGCSIHS
00675 ************************************************************      ELGCSIHS
00676 *                                                          *      ELGCSIHS
00677 *        FIND OCCURENCE OF DAILY ROOM AND BOARD PROVISION I*      ELGCSIHS
00678 *                                                          *      ELGCSIHS
00679 ************************************************************      ELGCSIHS
00680  FIND-OCCURENCE-OF-DAILY-ROOM-A.                                  ELGCSIHS
00681      IF CSBP-BP-KEY (CSBP-X-IDX) = WS-PROVISION-ID-KEY            ELGCSIHS
00682          PERFORM DETERMINE-DAILY-ROOM-AND-BOARD.                  ELGCSIHS
00683                                                                   ELGCSIHS
00684                                                                   ELGCSIHS
00685 ************************************************************      ELGCSIHS
00686 *                                                          *      ELGCSIHS
00687 *        DETERMINE DAILY ROOM AND BOARD COVERAGE           *      ELGCSIHS
00688 *                                                          *      ELGCSIHS
00689 ************************************************************      ELGCSIHS
00690  DETERMINE-DAILY-ROOM-AND-BOARD.                                  ELGCSIHS
00691      IF CSBP-COVERED (CSBP-X-IDX)                                 ELGCSIHS
00692          PERFORM SEARCH-FOR-BASIC-DAYS                            ELGCSIHS
00693      ELSE                                                         ELGCSIHS
00694          PERFORM DISPLAY-DAILY-ROOM-AND-BOARD-I.                  ELGCSIHS
00695      SET PROVISION-FOUND TO TRUE.                                 ELGCSIHS
00696                                                                   ELGCSIHS
00697 ************************************************************      ELGCSIHS
00698 *                                                          *      ELGCSIHS
00699 *        DISPLAY DAILY ROOM AND BOARD IS NOT COVERED       *      ELGCSIHS
00700 *                                                          *      ELGCSIHS
00701 ************************************************************      ELGCSIHS
00702  DISPLAY-DAILY-ROOM-AND-BOARD-I.                                  ELGCSIHS
00703      ADD +1 TO COF-NBR-DTL-LINES.                                 ELGCSIHS
00704      STRING WS-IP-DAYS-PHRASE         DELIMITED BY SIZE           ELGCSIHS
00705             WS-DRB-NOT-COVERED        DELIMITED BY SIZE           ELGCSIHS
00706             INTO COF-DTL-LINE (1).                                ELGCSIHS
00707      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSIHS
00708      SET  BASIC-UNLIMITED-FOUND       TO TRUE.                    ELGCSIHS
00709                                                                   ELGCSIHS
00710 ************************************************************      ELGCSIHS
00711 *                                                          *      ELGCSIHS
00712 *        CREATE BENEFIT AND INTERVAL LINES                 *      ELGCSIHS
00713 *                                                          *      ELGCSIHS
00714 ************************************************************      ELGCSIHS
00715  CREATE-BENEFIT-AND-INTERVAL-LI.                                  ELGCSIHS
00716      IF WS-BENEFIT-PERIOD NOT EQUAL ZEROS AND SPACES AND          ELGCSIHS
00717          LOW-VALUES                                               ELGCSIHS
00718          PERFORM CREATE-BENEFIT-PERIOD-LINE.                      ELGCSIHS
00719      IF WS-INTERVAL-TYPE  NOT EQUAL ZEROS AND SPACES AND          ELGCSIHS
00720          LOW-VALUES                                               ELGCSIHS
00721          PERFORM CREATE-INTERVAL-LINE.                            ELGCSIHS
00722                                                                   ELGCSIHS
00723                                                                   ELGCSIHS
00724 ************************************************************      ELGCSIHS
00725 *                                                          *      ELGCSIHS
00726 *        SEARCH FOR BASIC DAYS                             *      ELGCSIHS
00727 *                                                          *      ELGCSIHS
00728 ************************************************************      ELGCSIHS
00729  SEARCH-FOR-BASIC-DAYS.                                           ELGCSIHS
00730      PERFORM SEARCH-ABM-ACCUMS-FOR-BASIC-DA.                      ELGCSIHS
00731      IF BASIC-UNLIMITED OR BASIC-NOT-FOUND                        ELGCSIHS
00732          PERFORM INDICATE-UNLIMITED-FOR-INPATIE.                  ELGCSIHS
00733                                                                   ELGCSIHS
00734 ************************************************************      ELGCSIHS
00735 *                                                          *      ELGCSIHS
00736 *        INDICATE UNLIMITED FOR INPATIENT DAYS             *      ELGCSIHS
00737 *                                                          *      ELGCSIHS
00738 ************************************************************      ELGCSIHS
00739  INDICATE-UNLIMITED-FOR-INPATIE.                                  ELGCSIHS
00740      ADD +1                      TO COF-NBR-DTL-LINES.            ELGCSIHS
00741      STRING WS-IP-DAYS-PHRASE         DELIMITED BY SIZE           ELGCSIHS
00742             'BASIC-'                  DELIMITED BY SIZE           ELGCSIHS
00743             PC-UNLIMITED              DELIMITED BY SIZE           ELGCSIHS
00744             INTO COF-DTL-LINE (1).                                ELGCSIHS
00745      SET  BASIC-UNLIMITED-FOUND  TO TRUE.                         ELGCSIHS
00746      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSIHS
00747                                                                   ELGCSIHS
00748 ************************************************************      ELGCSIHS
00749 *                                                          *      ELGCSIHS
00750 *        PROCESS ALL OTHER DAYS                            *      ELGCSIHS
00751 *                                                          *      ELGCSIHS
00752 ************************************************************      ELGCSIHS
00753  PROCESS-ALL-OTHER-DAYS.                                          ELGCSIHS
00754      IF BASIC-FOUND                                               ELGCSIHS
00755          PERFORM INVESTIGATE-ECF-AND-CHC-DAYS.                    ELGCSIHS
00756      PERFORM SEARCH-FOR-EXCEPTED-DAYS.                            ELGCSIHS
00757      ADD +1                        TO                             ELGCSIHS
00758          COF-NBR-DTL-LINES.                                       ELGCSIHS
00759      MOVE PC-LEFT-SIDE-BAR-ONLY    TO COF-DTL-LINE                ELGCSIHS
00760          (COF-NBR-DTL-LINES).                                     ELGCSIHS
00761      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSIHS
00762                                                                   ELGCSIHS
00763                                                                   ELGCSIHS
00764 ************************************************************      ELGCSIHS
00765 *                                                          *      ELGCSIHS
00766 *        INVESTIGATE ECF AND CHC DAYS                      *      ELGCSIHS
00767 *                                                          *      ELGCSIHS
00768 ************************************************************      ELGCSIHS
00769  INVESTIGATE-ECF-AND-CHC-DAYS.                                    ELGCSIHS
00770      IF WS-GPA-DRR-IND = '1'                                      ELGCSIHS
00771          PERFORM CALCULATE-ECF-DAYS.                              ELGCSIHS
00772      INITIALIZE WS-PROVISION-SW.                                  ELGCSIHS
00773      PERFORM FIND-OCCURENCE-OF-CHC-IN-BP-MA                       ELGCSIHS
00774          VARYING CSBP-X-IDX FROM +1 BY +1                         ELGCSIHS
00775                 UNTIL   CSBP-X-IDX > CSBP-TBL-CNT                 ELGCSIHS
00776                 OR      PROVISION-FOUND.                          ELGCSIHS
00777      PERFORM GENERATE-FIRST-LINE.                                 ELGCSIHS
00778                                                                   ELGCSIHS
00779 ************************************************************      ELGCSIHS
00780 *                                                          *      ELGCSIHS
00781 *        FIND OCCURENCE OF CHC IN BP MATRIX                *      ELGCSIHS
00782 *                                                          *      ELGCSIHS
00783 ************************************************************      ELGCSIHS
00784  FIND-OCCURENCE-OF-CHC-IN-BP-MA.                                  ELGCSIHS
00785      IF CSBP-BP-KEY (CSBP-X-IDX) = PC-CHC-W                       ELGCSIHS
00786          PERFORM DETERMINE-CHC-COVERAGE.                          ELGCSIHS
00787                                                                   ELGCSIHS
00788                                                                   ELGCSIHS
00789 ************************************************************      ELGCSIHS
00790 *                                                          *      ELGCSIHS
00791 *        DETERMINE CHC COVERAGE                            *      ELGCSIHS
00792 *                                                          *      ELGCSIHS
00793 ************************************************************      ELGCSIHS
00794  DETERMINE-CHC-COVERAGE.                                          ELGCSIHS
00795      IF (WS-GPW-DRR-IND = '0' OR '2') AND CSBP-COVERED            ELGCSIHS
00796          (CSBP-X-IDX)                                             ELGCSIHS
00797          PERFORM CALCULATE-CHC-DAYS.                              ELGCSIHS
00798      SET PROVISION-FOUND TO TRUE.                                 ELGCSIHS
00799                                                                   ELGCSIHS
00800 ************************************************************      ELGCSIHS
00801 *                                                          *      ELGCSIHS
00802 *        SEARCH ABM ACCUMS FOR BASIC DAYS                  *      ELGCSIHS
00803 *                                                          *      ELGCSIHS
00804 ************************************************************      ELGCSIHS
00805  SEARCH-ABM-ACCUMS-FOR-BASIC-DA.                                  ELGCSIHS
00806      SET BASIC-NOT-FOUND TO TRUE.                                 ELGCSIHS
00807      IF CSAC-ABM-BP-TBL-PTR NOT = NULL                            ELGCSIHS
00808          PERFORM CHECK-BENEFIT-PROVISION-LEVELX.                  ELGCSIHS
00809      IF BASIC-NOT-FOUND AND                                       ELGCSIHS
00810                CSAC-ABM-GC-TBL-PTR NOT = NULL                     ELGCSIHS
00811          PERFORM CHECK-CONTRACT-LEVEL-ABM.                        ELGCSIHS
00812      IF BASIC-NOT-FOUND AND                                       ELGCSIHS
00813                CSAC-ABM-GC-TBL-PTR NOT = NULL                     ELGCSIHS
00814          PERFORM CHECK-GROUP-SPECIFIC-LEVEL-ABM.                  ELGCSIHS
00815      IF BASIC-FOUND                                               ELGCSIHS
00816          PERFORM OBTAIN-BENEFIT-PROVISION-RECOR.                  ELGCSIHS
00817                                                                   ELGCSIHS
00818                                                                   ELGCSIHS
00819 ************************************************************      ELGCSIHS
00820 *                                                          *      ELGCSIHS
00821 *        CHECK BENEFIT PROVISION LEVEL ABM                 *      ELGCSIHS
00822 *                                                          *      ELGCSIHS
00823 ************************************************************      ELGCSIHS
00824  CHECK-BENEFIT-PROVISION-LEVELX.                                  ELGCSIHS
00825      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                     ELGCSIHS
00826            CSAC-ABM-BP-TBL-PTR.                                   ELGCSIHS
00827      MOVE CSBP-BP-ABM-SLOT (CSBP-X-IDX) TO                        ELGCSIHS
00828          WS-PROVISION-SLOT-NO.                                    ELGCSIHS
00829      IF WS-PROVISION-SLOT-NO > ZERO                               ELGCSIHS
00830          PERFORM PROCESS-ABM-TABULAR.                             ELGCSIHS
00831                                                                   ELGCSIHS
00832 ************************************************************      ELGCSIHS
00833 *                                                          *      ELGCSIHS
00834 *        CHECK GROUP SPECIFIC LEVEL ABM                    *      ELGCSIHS
00835 *                                                          *      ELGCSIHS
00836 ************************************************************      ELGCSIHS
00837  CHECK-GROUP-SPECIFIC-LEVEL-ABM.                                  ELGCSIHS
00838      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                     ELGCSIHS
00839            CSAC-ABM-GC-TBL-PTR.                                   ELGCSIHS
00840      MOVE  WS-HOLD-GROUP-ABM-SLOT       TO                        ELGCSIHS
00841          WS-PROVISION-SLOT-NO.                                    ELGCSIHS
00842      IF WS-PROVISION-SLOT-NO > ZERO                               ELGCSIHS
00843          PERFORM PROCESS-ABM-TABULAR.                             ELGCSIHS
00844                                                                   ELGCSIHS
00845 ************************************************************      ELGCSIHS
00846 *                                                          *      ELGCSIHS
00847 *        CHECK CONTRACT LEVEL ABM                          *      ELGCSIHS
00848 *                                                          *      ELGCSIHS
00849 ************************************************************      ELGCSIHS
00850  CHECK-CONTRACT-LEVEL-ABM.                                        ELGCSIHS
00851      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                     ELGCSIHS
00852            CSAC-ABM-GC-TBL-PTR.                                   ELGCSIHS
00853      MOVE  WS-HOLD-CONTRACT-ABM-SLOT    TO                        ELGCSIHS
00854          WS-PROVISION-SLOT-NO.                                    ELGCSIHS
00855      IF WS-PROVISION-SLOT-NO > ZERO                               ELGCSIHS
00856          PERFORM PROCESS-ABM-TABULAR.                             ELGCSIHS
00857                                                                   ELGCSIHS
00858 ************************************************************      ELGCSIHS
00859 *                                                          *      ELGCSIHS
00860 *        SETUP IOP PARAMETERS                              *      ELGCSIHS
00861 *                                                          *      ELGCSIHS
00862 ************************************************************      ELGCSIHS
00863  SETUP-IOP-PARAMETERS.                                            ELGCSIHS
00864      SET  CIA-GCTABULR-DDN    TO  TRUE.                           ELGCSIHS
00865      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00866          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGCSIHS
00867      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00868          PERFORM OBTAIN-STORAGE-FOR-IO-AREA.                      ELGCSIHS
00869      SET  CIA-GCTABULR-DDN    TO  TRUE.                           ELGCSIHS
00870      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00871          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGCSIHS
00872      SET IOP-RD               TO  TRUE.                           ELGCSIHS
00873      SET IOP-FCQ-NONE         TO  TRUE.                           ELGCSIHS
00874      SET IOP-KVQ-EQ           TO  TRUE.                           ELGCSIHS
00875      MOVE KWA-GCTABULR-KEY    TO  IOP-FILE-KEY.                   ELGCSIHS
00876      MOVE SPACES              TO  IOP-AIX-DDNAME.                 ELGCSIHS
00877                                                                   ELGCSIHS
00878                                                                   ELGCSIHS
00879 ************************************************************      ELGCSIHS
00880 *                                                          *      ELGCSIHS
00881 *        SIGNAL GCTABULR NOT FOUND ABEND                   *      ELGCSIHS
00882 *                                                          *      ELGCSIHS
00883 ************************************************************      ELGCSIHS
00884  SIGNAL-GCTABULR-NOT-FOUND-ABEN.                                  ELGCSIHS
00885      SET CIA-AB-NOTFND-GCTABULR  TO TRUE.                         ELGCSIHS
00886      PERFORM SIGNAL-ABEND.                                        ELGCSIHS
00887                                                                   ELGCSIHS
00888 ************************************************************      ELGCSIHS
00889 *                                                          *      ELGCSIHS
00890 *        OBTAIN STORAGE FOR IO AREA                        *      ELGCSIHS
00891 *                                                          *      ELGCSIHS
00892 ************************************************************      ELGCSIHS
00893  OBTAIN-STORAGE-FOR-IO-AREA.                                      ELGCSIHS
00894      SET CIA-STG-GETMAIN TO TRUE.                                 ELGCSIHS
00895      PERFORM CALL-STORAGE-MANAGER.                                ELGCSIHS
00896                                                                   ELGCSIHS
00897 ************************************************************      ELGCSIHS
00898 *                                                          *      ELGCSIHS
00899 *        CALL STORAGE MANAGER                              *      ELGCSIHS
00900 *                                                          *      ELGCSIHS
00901 ************************************************************      ELGCSIHS
00902  CALL-STORAGE-MANAGER.                                            ELGCSIHS
00903      EXEC CICS LINK                                               ELGCSIHS
00904                PROGRAM ('ELUSTGMG')                               ELGCSIHS
00905                COMMAREA (DFHCOMMAREA)                             ELGCSIHS
00906         END-EXEC.                                                 ELGCSIHS
00907                                                                   ELGCSIHS
00908 ************************************************************      ELGCSIHS
00909 *                                                          *      ELGCSIHS
00910 *        SIGNAL CRITICAL IO ERROR                          *      ELGCSIHS
00911 *                                                          *      ELGCSIHS
00912 ************************************************************      ELGCSIHS
00913  SIGNAL-CRITICAL-IO-ERROR.                                        ELGCSIHS
00914      SET CIA-AB-CRITIO  TO  TRUE.                                 ELGCSIHS
00915      PERFORM SIGNAL-ABEND.                                        ELGCSIHS
00916                                                                   ELGCSIHS
00917 ************************************************************      ELGCSIHS
00918 *                                                          *      ELGCSIHS
00919 *        OBTAIN BENEFIT PROVISION RECORDS FOR ECF AND CHC F*      ELGCSIHS
00920 *                                                          *      ELGCSIHS
00921 ************************************************************      ELGCSIHS
00922  OBTAIN-BENEFIT-PROVISION-RECOR.                                  ELGCSIHS
00923      IF ADDRESS OF CONTRACT-RECORD = NULL                         ELGCSIHS
00924          PERFORM SIGNAL-MISSING-CONTRACT.                         ELGCSIHS
00925      PERFORM FIND-BENEFIT-PROVISION-RECORDS                       ELGCSIHS
00926          VARYING GCT-INDEX FROM +1 BY +1                          ELGCSIHS
00927                 UNTIL   GCT-INDEX =                               ELGCSIHS
00928              GCT-COUNT-BEN-PROVN-POINTERS                         ELGCSIHS
00929                 OR      GCT-BEN-PROVN-ID (GCT-INDEX) >            ELGCSIHS
00930              WS-PROVISION-ID-KEY.                                 ELGCSIHS
00931                                                                   ELGCSIHS
00932 ************************************************************      ELGCSIHS
00933 *                                                          *      ELGCSIHS
00934 *        FIND BENEFIT PROVISION RECORDS NEEDED             *      ELGCSIHS
00935 *                                                          *      ELGCSIHS
00936 ************************************************************      ELGCSIHS
00937  FIND-BENEFIT-PROVISION-RECORDS.                                  ELGCSIHS
00938      IF GCT-BEN-PROVN-ID (GCT-INDEX) = WS-PROVISION-ID-KEY        ELGCSIHS
00939          AND                                                      ELGCSIHS
00940                GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) > +0             ELGCSIHS
00941          PERFORM READ-THE-DAILY-ROOM-AND-BOARDX.                  ELGCSIHS
00942      IF GCT-BEN-PROVN-ID (GCT-INDEX) = PC-CHC-W AND               ELGCSIHS
00943                GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) > +0             ELGCSIHS
00944          PERFORM READ-THE-COORDINATED-HOME-CARE.                  ELGCSIHS
00945                                                                   ELGCSIHS
00946 ************************************************************      ELGCSIHS
00947 *                                                          *      ELGCSIHS
00948 *        READ THE DAILY ROOM AND BOARD RECORD AND HOLD FIEL*      ELGCSIHS
00949 *                                                          *      ELGCSIHS
00950 ************************************************************      ELGCSIHS
00951  READ-THE-DAILY-ROOM-AND-BOARDX.                                  ELGCSIHS
00952      MOVE WS-PROVISION-ID-KEY               TO                    ELGCSIHS
00953          KWA-GCP-PROVN-ID.                                        ELGCSIHS
00954      PERFORM READ-THE-BENEFIT-PROVISION-REC.                      ELGCSIHS
00955      PERFORM HOLD-DAYS-REDUCTION-RATE-INFOA.                      ELGCSIHS
00956                                                                   ELGCSIHS
00957 ************************************************************      ELGCSIHS
00958 *                                                          *      ELGCSIHS
00959 *        READ THE COORDINATED HOME CARE RECORD AND HOLD FIE*      ELGCSIHS
00960 *                                                          *      ELGCSIHS
00961 ************************************************************      ELGCSIHS
00962  READ-THE-COORDINATED-HOME-CARE.                                  ELGCSIHS
00963      MOVE PC-CHC-W                          TO KWA-GCP-PROVN-ID.  ELGCSIHS
00964      PERFORM READ-THE-BENEFIT-PROVISION-REC.                      ELGCSIHS
00965      PERFORM HOLD-DAYS-REDUCTION-RATE-INFOX.                      ELGCSIHS
00966                                                                   ELGCSIHS
00967 ************************************************************      ELGCSIHS
00968 *                                                          *      ELGCSIHS
00969 *        READ THE BENEFIT PROVISION RECORD GIVEN           *      ELGCSIHS
00970 *                                                          *      ELGCSIHS
00971 ************************************************************      ELGCSIHS
00972  READ-THE-BENEFIT-PROVISION-REC.                                  ELGCSIHS
00973      SET  CIA-GCBENPRV-DDN                  TO TRUE.              ELGCSIHS
00974      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00975          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGCSIHS
00976      IF CIA-RC-PTR-NULL                                           ELGCSIHS
00977          PERFORM OBTAIN-STORAGE-FOR-IO-AREA.                      ELGCSIHS
00978      SET  CIA-GCBENPRV-DDN                  TO TRUE.              ELGCSIHS
00979      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
00980          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELGCSIHS
00981      SET IOP-RD                             TO TRUE.              ELGCSIHS
00982      SET IOP-FCQ-NONE                       TO TRUE.              ELGCSIHS
00983      SET IOP-KVQ-EQ                         TO TRUE.              ELGCSIHS
00984      SET IOP-STG-MODE-LOCATE                TO TRUE.              ELGCSIHS
00985      MOVE GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) TO                    ELGCSIHS
00986          KWA-GCP-PROVN-SLOT-NO.                                   ELGCSIHS
00987      MOVE KWA-GCBENPRV-KEY                  TO IOP-FILE-KEY.      ELGCSIHS
00988      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELGCSIHS
00989      IF IOP-RC-OK                                                 ELGCSIHS
00990          PERFORM ESTABLISH-ADDRESS-OF-BP-RECORD                   ELGCSIHS
00991      ELSE                                                         ELGCSIHS
00992          PERFORM SIGNAL-CRITICAL-IO-ERROR.                        ELGCSIHS
00993                                                                   ELGCSIHS
00994                                                                   ELGCSIHS
00995 ************************************************************      ELGCSIHS
00996 *                                                          *      ELGCSIHS
00997 *        HOLD DAYS REDUCTION RATE INFO FOR A-FORMAT        *      ELGCSIHS
00998 *                                                          *      ELGCSIHS
00999 ************************************************************      ELGCSIHS
01000  HOLD-DAYS-REDUCTION-RATE-INFOA.                                  ELGCSIHS
01001      MOVE GPA-DAYS-RDCN-RAT-IND           TO                      ELGCSIHS
01002          WS-GPA-DRR-IND.                                          ELGCSIHS
01003      MOVE GPA-DAYS-RDCN-RAT-BASIC-APL     TO                      ELGCSIHS
01004          WS-GPA-DRR-BASIC-APL.                                    ELGCSIHS
01005      MOVE GPA-DAYS-RDCN-RAT-BASIC-BASE    TO                      ELGCSIHS
01006          WS-GPA-DRR-BASIC-BASE.                                   ELGCSIHS
01007                                                                   ELGCSIHS
01008                                                                   ELGCSIHS
01009 ************************************************************      ELGCSIHS
01010 *                                                          *      ELGCSIHS
01011 *        HOLD DAYS REDUCTION RATE INFO FOR W-FORMAT        *      ELGCSIHS
01012 *                                                          *      ELGCSIHS
01013 ************************************************************      ELGCSIHS
01014  HOLD-DAYS-REDUCTION-RATE-INFOX.                                  ELGCSIHS
01015      MOVE GPW-DAYS-RDCN-RAT-IND           TO WS-GPW-DRR-IND.      ELGCSIHS
01016      MOVE GPW-DAYS-RDCN-RAT-BASIC-APL     TO                      ELGCSIHS
01017          WS-GPW-DRR-BASIC-APL.                                    ELGCSIHS
01018      MOVE GPW-DAYS-RDCN-RAT-BASIC-BASE    TO                      ELGCSIHS
01019          WS-GPW-DRR-BASIC-BASE.                                   ELGCSIHS
01020                                                                   ELGCSIHS
01021 ************************************************************      ELGCSIHS
01022 *                                                          *      ELGCSIHS
01023 *        ESTABLISH ADDRESS OF BP RECORD                    *      ELGCSIHS
01024 *                                                          *      ELGCSIHS
01025 ************************************************************      ELGCSIHS
01026  ESTABLISH-ADDRESS-OF-BP-RECORD.                                  ELGCSIHS
01027      SET ADDRESS OF BENEFIT-PROVISION-RECORD TO IOP-REC-PTR.      ELGCSIHS
01028                                                                   ELGCSIHS
01029 ************************************************************      ELGCSIHS
01030 *                                                          *      ELGCSIHS
01031 *        PROCESS ABM TABULAR                               *      ELGCSIHS
01032 *                                                          *      ELGCSIHS
01033 ************************************************************      ELGCSIHS
01034  PROCESS-ABM-TABULAR.                                             ELGCSIHS
01035      PERFORM CHECK-IF-OCCURENCE-QUALIFIES                         ELGCSIHS
01036          VARYING WS-ATBL-X-SUB FROM +1 BY +1                      ELGCSIHS
01037                 UNTIL   WS-ATBL-X-SUB > ATBL-TBL-CNT.             ELGCSIHS
01038                                                                   ELGCSIHS
01039 ************************************************************      ELGCSIHS
01040 *                                                          *      ELGCSIHS
01041 *        CHECK IF OCCURENCE QUALIFIES                      *      ELGCSIHS
01042 *                                                          *      ELGCSIHS
01043 ************************************************************      ELGCSIHS
01044  CHECK-IF-OCCURENCE-QUALIFIES.                                    ELGCSIHS
01045      SET ATBL-X-IDX TO WS-ATBL-X-SUB.                             ELGCSIHS
01046      IF ATBL-SLOT-NUMBER (ATBL-X-IDX) =                           ELGCSIHS
01047          WS-PROVISION-SLOT-NO                                     ELGCSIHS
01048            AND SSB-INST-BAS-L-O-B EQUAL ATBL-L-O-B                ELGCSIHS
01049          (ATBL-X-IDX)                                             ELGCSIHS
01050          PERFORM SUMMARIZE-ACCUMULATOR-OCCURENC.                  ELGCSIHS
01051                                                                   ELGCSIHS
01052 ************************************************************      ELGCSIHS
01053 *                                                          *      ELGCSIHS
01054 *        SUMMARIZE ACCUMULATOR OCCURENCE                   *      ELGCSIHS
01055 *                                                          *      ELGCSIHS
01056 ************************************************************      ELGCSIHS
01057  SUMMARIZE-ACCUMULATOR-OCCURENC.                                  ELGCSIHS
01058      INITIALIZE              WS-ABM-OCCURENCE.                    ELGCSIHS
01059      SET VALID-IP-FIELD      TO TRUE.                             ELGCSIHS
01060      PERFORM EXAMINE-VALUE-QUALIFIER-FIELD.                       ELGCSIHS
01061      IF VALID-IP-FIELD                                            ELGCSIHS
01062          PERFORM EXAMINE-PLACE-OF-TREATMENT-FIE.                  ELGCSIHS
01063      IF VALID-IP-FIELD                                            ELGCSIHS
01064          PERFORM EXAMINE-INTERNAL-DESCRIPTOR.                     ELGCSIHS
01065      IF VALID-IP-FIELD                                            ELGCSIHS
01066          PERFORM DETERMINE-WHAT-DAYS-ARE-PRESEN.                  ELGCSIHS
01067                                                                   ELGCSIHS
01068 ************************************************************      ELGCSIHS
01069 *                                                          *      ELGCSIHS
01070 *        EXAMINE VALUE QUALIFIER FIELD                     *      ELGCSIHS
01071 *                                                          *      ELGCSIHS
01072 ************************************************************      ELGCSIHS
01073  EXAMINE-VALUE-QUALIFIER-FIELD.                                   ELGCSIHS
01074      IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) NOT = '3'               ELGCSIHS
01075          PERFORM SET-INVALID-INPATIENT-FIELD.                     ELGCSIHS
01076                                                                   ELGCSIHS
01077 ************************************************************      ELGCSIHS
01078 *                                                          *      ELGCSIHS
01079 *        EXAMINE PLACE OF TREATMENT FIELD                  *      ELGCSIHS
01080 *                                                          *      ELGCSIHS
01081 ************************************************************      ELGCSIHS
01082  EXAMINE-PLACE-OF-TREATMENT-FIE.                                  ELGCSIHS
01083      MOVE ATBL-PLACE-OF-TREATMENT (ATBL-X-IDX)                    ELGCSIHS
01084                          TO WS-PLACE-OF-TREATMENT.                ELGCSIHS
01085      IF NOT INPATIENT-POT                                         ELGCSIHS
01086          PERFORM SET-INVALID-INPATIENT-FIELD.                     ELGCSIHS
01087                                                                   ELGCSIHS
01088 ************************************************************      ELGCSIHS
01089 *                                                          *      ELGCSIHS
01090 *        SET INVALID INPATIENT FIELD                       *      ELGCSIHS
01091 *                                                          *      ELGCSIHS
01092 ************************************************************      ELGCSIHS
01093  SET-INVALID-INPATIENT-FIELD.                                     ELGCSIHS
01094      SET INVALID-IP-FIELD TO TRUE.                                ELGCSIHS
01095                                                                   ELGCSIHS
01096 ************************************************************      ELGCSIHS
01097 *                                                          *      ELGCSIHS
01098 *        EXAMINE INTERNAL DESCRIPTOR                       *      ELGCSIHS
01099 *                                                          *      ELGCSIHS
01100 ************************************************************      ELGCSIHS
01101  EXAMINE-INTERNAL-DESCRIPTOR.                                     ELGCSIHS
01102      IF ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX) NOT =               ELGCSIHS
01103          'DAYREDUCE'                                              ELGCSIHS
01104          PERFORM SET-INVALID-INPATIENT-FIELD.                     ELGCSIHS
01105                                                                   ELGCSIHS
01106 ************************************************************      ELGCSIHS
01107 *                                                          *      ELGCSIHS
01108 *        DETERMINE WHAT DAYS ARE PRESENT                   *      ELGCSIHS
01109 *                                                          *      ELGCSIHS
01110 ************************************************************      ELGCSIHS
01111  DETERMINE-WHAT-DAYS-ARE-PRESEN.                                  ELGCSIHS
01112      MOVE ATBL-CONDITION (ATBL-X-IDX)          TO                 ELGCSIHS
01113          WS-CONDITION-BITS.                                       ELGCSIHS
01114      IF (BITS-NO-EXCEPT OR BITS-WITH-EXCEPT) AND                  ELGCSIHS
01115          BASIC-NOT-FOUND                                          ELGCSIHS
01116          PERFORM EXAMINE-INTERNAL-TABULARS                        ELGCSIHS
01117      ELSE IF BITS-FOR-INDIVIDUAL         AND                      ELGCSIHS
01118                BASIC-PROCESS-FINISHED      AND                    ELGCSIHS
01119               (MENTAL OR DRUG OR ALCOHOL)                         ELGCSIHS
01120          PERFORM PROCESS-EXCEPTIONS-IF-APPLICAB.                  ELGCSIHS
01121                                                                   ELGCSIHS
01122 ************************************************************      ELGCSIHS
01123 *                                                          *      ELGCSIHS
01124 *        EXAMINE INTERNAL TABULARS                         *      ELGCSIHS
01125 *                                                          *      ELGCSIHS
01126 ************************************************************      ELGCSIHS
01127  EXAMINE-INTERNAL-TABULARS.                                       ELGCSIHS
01128      IF ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX) > ZERO                 ELGCSIHS
01129          PERFORM PROCESS-IBGR-TABULAR.                            ELGCSIHS
01130                                                                   ELGCSIHS
01131 ************************************************************      ELGCSIHS
01132 *                                                          *      ELGCSIHS
01133 *        PROCESS IBGR TABULAR                              *      ELGCSIHS
01134 *                                                          *      ELGCSIHS
01135 ************************************************************      ELGCSIHS
01136  PROCESS-IBGR-TABULAR.                                            ELGCSIHS
01137      PERFORM READ-IBGR-TABULAR.                                   ELGCSIHS
01138      IF GX1-ID-ARGUMENT-INCLUDED                                  ELGCSIHS
01139          PERFORM CHECK-IF-IBGR-OCCURENCE-QUALIF.                  ELGCSIHS
01140                                                                   ELGCSIHS
01141 ************************************************************      ELGCSIHS
01142 *                                                          *      ELGCSIHS
01143 *        CHECK IF IBGR OCCURENCE QUALIFIES                 *      ELGCSIHS
01144 *                                                          *      ELGCSIHS
01145 ************************************************************      ELGCSIHS
01146  CHECK-IF-IBGR-OCCURENCE-QUALIF.                                  ELGCSIHS
01147      INITIALIZE WS-PROVISION-SW.                                  ELGCSIHS
01148      PERFORM DETERMINE-IF-PROVISION-IS-INCL                       ELGCSIHS
01149          VARYING GX1-INDEX FROM +1 BY +1                          ELGCSIHS
01150                 UNTIL   GX1-INDEX = GX1-ENTRY-COUNT               ELGCSIHS
01151                 OR      GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) >   ELGCSIHS
01152                         WS-PROVISION-ID-KEY                       ELGCSIHS
01153                 OR      PROVISION-FOUND.                          ELGCSIHS
01154      IF PROVISION-FOUND                                           ELGCSIHS
01155          PERFORM SAVE-BASIC-DAYS-AND-HOLD-NECES.                  ELGCSIHS
01156                                                                   ELGCSIHS
01157 ************************************************************      ELGCSIHS
01158 *                                                          *      ELGCSIHS
01159 *        DETERMINE IF PROVISION IS INCLUDED                *      ELGCSIHS
01160 *                                                          *      ELGCSIHS
01161 ************************************************************      ELGCSIHS
01162  DETERMINE-IF-PROVISION-IS-INCL.                                  ELGCSIHS
01163      IF GX1-PROVISION-ID-ARGUMENT (GX1-INDEX) =                   ELGCSIHS
01164          WS-PROVISION-ID-KEY                                      ELGCSIHS
01165          PERFORM INDICATE-PROVISION-IS-FOUND.                     ELGCSIHS
01166                                                                   ELGCSIHS
01167 ************************************************************      ELGCSIHS
01168 *                                                          *      ELGCSIHS
01169 *        INDICATE PROVISION IS FOUND                       *      ELGCSIHS
01170 *                                                          *      ELGCSIHS
01171 ************************************************************      ELGCSIHS
01172  INDICATE-PROVISION-IS-FOUND.                                     ELGCSIHS
01173      SET PROVISION-FOUND      TO TRUE.                            ELGCSIHS
01174                                                                   ELGCSIHS
01175 ************************************************************      ELGCSIHS
01176 *                                                          *      ELGCSIHS
01177 *        READ IBGR TABULAR                                 *      ELGCSIHS
01178 *                                                          *      ELGCSIHS
01179 ************************************************************      ELGCSIHS
01180  READ-IBGR-TABULAR.                                               ELGCSIHS
01181      MOVE PC-IBGR             TO KWA-PROVISION-ID.                ELGCSIHS
01182      MOVE ATBL-IBGR-SLOT-NUMBER (ATBL-X-IDX)                      ELGCSIHS
01183                               TO KWA-PROVISION-SLOT-NO.           ELGCSIHS
01184      PERFORM SETUP-IOP-PARAMETERS.                                ELGCSIHS
01185      SET IOP-STG-MODE-MOVE    TO  TRUE.                           ELGCSIHS
01186      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELGCSIHS
01187      IF IOP-RC-OK                                                 ELGCSIHS
01188          PERFORM ESTABLISH-ADDRESS-OF-IBGR-TABU                   ELGCSIHS
01189      ELSE                                                         ELGCSIHS
01190          PERFORM SIGNAL-GCTABULR-NOT-FOUND-ABEN.                  ELGCSIHS
01191                                                                   ELGCSIHS
01192 ************************************************************      ELGCSIHS
01193 *                                                          *      ELGCSIHS
01194 *        ESTABLISH ADDRESS OF IBGR TABULAR                 *      ELGCSIHS
01195 *                                                          *      ELGCSIHS
01196 ************************************************************      ELGCSIHS
01197  ESTABLISH-ADDRESS-OF-IBGR-TABU.                                  ELGCSIHS
01198      SET ADDRESS OF IBGR-RECORD  TO IOP-REC-PTR.                  ELGCSIHS
01199      SET IOP-REC-PTR             TO NULLS.                        ELGCSIHS
01200                                                                   ELGCSIHS
01201 ************************************************************      ELGCSIHS
01202 *                                                          *      ELGCSIHS
01203 *        SAVE BASIC DAYS AND HOLD NECESSARY FIELDS ON THAT *      ELGCSIHS
01204 *                                                          *      ELGCSIHS
01205 ************************************************************      ELGCSIHS
01206  SAVE-BASIC-DAYS-AND-HOLD-NECES.                                  ELGCSIHS
01207      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)       TO                  ELGCSIHS
01208          WS-BASIC-VALUE.                                          ELGCSIHS
01209      MOVE ATBL-BENEFIT-PERIOD (ATBL-X-IDX) TO WS-BENEFIT-PERIOD.  ELGCSIHS
01210      MOVE ATBL-INTERVAL-TIME-FCTR (ATBL-X-IDX)                    ELGCSIHS
01211                                               TO                  ELGCSIHS
01212          WS-INTERVAL-TIME-FACTOR.                                 ELGCSIHS
01213      MOVE ATBL-INTERVAL-TYPE (ATBL-X-IDX)     TO                  ELGCSIHS
01214          WS-INTERVAL-TYPE.                                        ELGCSIHS
01215      SET  BASIC-FOUND                         TO TRUE.            ELGCSIHS
01216                                                                   ELGCSIHS
01217 ************************************************************      ELGCSIHS
01218 *                                                          *      ELGCSIHS
01219 *        PROCESS EXCEPTIONS IF APPLICABLE                  *      ELGCSIHS
01220 *                                                          *      ELGCSIHS
01221 ************************************************************      ELGCSIHS
01222  PROCESS-EXCEPTIONS-IF-APPLICAB.                                  ELGCSIHS
01223      IF (MENTAL AND NOT MENTAL-FOUND)                             ELGCSIHS
01224          PERFORM INSERT-MENTAL-DAYS.                              ELGCSIHS
01225      IF (DRUG AND NOT DRUG-FOUND)                                 ELGCSIHS
01226          PERFORM INSERT-DRUG-DAYS.                                ELGCSIHS
01227      IF (ALCOHOL AND NOT ALCOHOL-FOUND)                           ELGCSIHS
01228          PERFORM INSERT-ALCOHOL-DAYS.                             ELGCSIHS
01229                                                                   ELGCSIHS
01230 ************************************************************      ELGCSIHS
01231 *                                                          *      ELGCSIHS
01232 *        CALL INPUT OUTPUT MODULE                          *      ELGCSIHS
01233 *                                                          *      ELGCSIHS
01234 ************************************************************      ELGCSIHS
01235  CALL-INPUT-OUTPUT-MODULE.                                        ELGCSIHS
01236      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELGCSIHS
01237                     COMMAREA (DFHCOMMAREA)                        ELGCSIHS
01238                     END-EXEC.                                     ELGCSIHS
01239                                                                   ELGCSIHS
01240 ************************************************************      ELGCSIHS
01241 *                                                          *      ELGCSIHS
01242 *        CALL OUTPUT INTERFACE                             *      ELGCSIHS
01243 *                                                          *      ELGCSIHS
01244 ************************************************************      ELGCSIHS
01245  CALL-OUTPUT-INTERFACE.                                           ELGCSIHS
01246      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELGCSIHS
01247                     COMMAREA (DFHCOMMAREA)                        ELGCSIHS
01248                     END-EXEC.                                     ELGCSIHS
01249                                                                   ELGCSIHS
01250 ************************************************************      ELGCSIHS
01251 *                                                          *      ELGCSIHS
01252 *        CALCULATE ECF DAYS                                *      ELGCSIHS
01253 *                                                          *      ELGCSIHS
01254 ************************************************************      ELGCSIHS
01255  CALCULATE-ECF-DAYS.                                              ELGCSIHS
01256      COMPUTE WS-ECF-VALUE  = (WS-GPA-DRR-BASIC-APL  *             ELGCSIHS
01257                               WS-GPA-DRR-BASIC-BASE *             ELGCSIHS
01258                               WS-BASIC-VALUE).                    ELGCSIHS
01259                                                                   ELGCSIHS
01260 ************************************************************      ELGCSIHS
01261 *                                                          *      ELGCSIHS
01262 *        CALCULATE CHC DAYS                                *      ELGCSIHS
01263 *                                                          *      ELGCSIHS
01264 ************************************************************      ELGCSIHS
01265  CALCULATE-CHC-DAYS.                                              ELGCSIHS
01266      COMPUTE WS-CHC-VALUE  = (WS-GPW-DRR-BASIC-APL  *             ELGCSIHS
01267                               WS-GPW-DRR-BASIC-BASE *             ELGCSIHS
01268                               WS-BASIC-VALUE).                    ELGCSIHS
01269                                                                   ELGCSIHS
01270 ************************************************************      ELGCSIHS
01271 *                                                          *      ELGCSIHS
01272 *        GENERATE FIRST LINE                               *      ELGCSIHS
01273 *                                                          *      ELGCSIHS
01274 ************************************************************      ELGCSIHS
01275  GENERATE-FIRST-LINE.                                             ELGCSIHS
01276      MOVE WS-BASIC-VALUE           TO WS-BASIC-DAYS.              ELGCSIHS
01277      IF WS-ECF-VALUE = ZERO AND WS-CHC-VALUE = ZERO               ELGCSIHS
01278          PERFORM GENERATE-BASIC-ONLY-ON-FIRST-L                   ELGCSIHS
01279      ELSE                                                         ELGCSIHS
01280          PERFORM DETERMINE-IF-ECF-AND-CHC-APPLY.                  ELGCSIHS
01281      PERFORM CALL-TEXT-COMPRESSION-MODULE.                        ELGCSIHS
01282      PERFORM SETUP-FOR-UNSTRING-OPERATION.                        ELGCSIHS
01283      PERFORM CALL-TEXT-UNSTRING-MODULE.                           ELGCSIHS
01284      MOVE WS-IP-DAYS-PHRASE        TO WS-TITLE.                   ELGCSIHS
01285      PERFORM MOVE-FORMATTED-TEXT-TO-OUTPUTX                       ELGCSIHS
01286          VARYING TCAR-X FROM +1 BY +1                             ELGCSIHS
01287                 UNTIL   TCAR-X > TCAR-OUTPUT-FIELDS-USED.         ELGCSIHS
01288      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSIHS
01289      PERFORM INITIALIZE-TEXT-COMPRESSION-AR.                      ELGCSIHS
01290      EJECT                                                        ELGCSIHS
01291                                                                   ELGCSIHS
01292                                                                   ELGCSIHS
01293 ************************************************************      ELGCSIHS
01294 *                                                          *      ELGCSIHS
01295 *        GENERATE BASIC ONLY ON FIRST LINE                 *      ELGCSIHS
01296 *                                                          *      ELGCSIHS
01297 ************************************************************      ELGCSIHS
01298  GENERATE-BASIC-ONLY-ON-FIRST-L.                                  ELGCSIHS
01299      ADD  +1                  TO TCAR-FROM-SUB.                   ELGCSIHS
01300      MOVE  WS-BASIC-PHRASE    TO TCAR-FROM-LINE                   ELGCSIHS
01301          (TCAR-FROM-SUB).                                         ELGCSIHS
01302                                                                   ELGCSIHS
01303 ************************************************************      ELGCSIHS
01304 *                                                          *      ELGCSIHS
01305 *        DETERMINE IF ECF AND CHC APPLY                    *      ELGCSIHS
01306 *                                                          *      ELGCSIHS
01307 ************************************************************      ELGCSIHS
01308  DETERMINE-IF-ECF-AND-CHC-APPLY.                                  ELGCSIHS
01309      MOVE WS-ECF-VALUE        TO WS-ECF-DAYS.                     ELGCSIHS
01310      MOVE WS-CHC-VALUE        TO WS-CHC-DAYS.                     ELGCSIHS
01311      IF (WS-ECF-VALUE > +0) AND (WS-CHC-VALUE > +0)               ELGCSIHS
01312          PERFORM DISPLAY-BASIC-ECF-AND-CHC-INFO                   ELGCSIHS
01313      ELSE IF WS-ECF-VALUE > +0                                    ELGCSIHS
01314          PERFORM DISPLAY-BASIC-AND-ECF                            ELGCSIHS
01315      ELSE IF WS-CHC-VALUE > +0                                    ELGCSIHS
01316          PERFORM DISPLAY-BASIC-AND-CHC.                           ELGCSIHS
01317                                                                   ELGCSIHS
01318 ************************************************************      ELGCSIHS
01319 *                                                          *      ELGCSIHS
01320 *        DISPLAY BASIC ECF AND CHC INFO                    *      ELGCSIHS
01321 *                                                          *      ELGCSIHS
01322 ************************************************************      ELGCSIHS
01323  DISPLAY-BASIC-ECF-AND-CHC-INFO.                                  ELGCSIHS
01324      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01325      MOVE WS-BASIC-PHRASE     TO TCAR-FROM-LINE                   ELGCSIHS
01326          (TCAR-FROM-SUB).                                         ELGCSIHS
01327      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01328      MOVE WS-ECF-PHRASE       TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGCSIHS
01329      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01330      MOVE WS-CHC-PHRASE       TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGCSIHS
01331      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01332      MOVE ' |'                TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGCSIHS
01333                                                                   ELGCSIHS
01334 ************************************************************      ELGCSIHS
01335 *                                                          *      ELGCSIHS
01336 *        DISPLAY BASIC AND ECF                             *      ELGCSIHS
01337 *                                                          *      ELGCSIHS
01338 ************************************************************      ELGCSIHS
01339  DISPLAY-BASIC-AND-ECF.                                           ELGCSIHS
01340      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01341      MOVE WS-BASIC-PHRASE     TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGCSIHS
01342      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01343      MOVE WS-ECF-PHRASE       TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGCSIHS
01344      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01345      MOVE ' |'                TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGCSIHS
01346                                                                   ELGCSIHS
01347 ************************************************************      ELGCSIHS
01348 *                                                          *      ELGCSIHS
01349 *        DISPLAY BASIC AND CHC                             *      ELGCSIHS
01350 *                                                          *      ELGCSIHS
01351 ************************************************************      ELGCSIHS
01352  DISPLAY-BASIC-AND-CHC.                                           ELGCSIHS
01353      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01354      MOVE WS-BASIC-PHRASE     TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGCSIHS
01355      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01356      MOVE WS-CHC-PHRASE       TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGCSIHS
01357      ADD +1                   TO TCAR-FROM-SUB.                   ELGCSIHS
01358      MOVE ' |'                TO TCAR-FROM-LINE (TCAR-FROM-SUB).  ELGCSIHS
01359                                                                   ELGCSIHS
01360 ************************************************************      ELGCSIHS
01361 *                                                          *      ELGCSIHS
01362 *        SEARCH FOR EXCEPTED DAYS                          *      ELGCSIHS
01363 *                                                          *      ELGCSIHS
01364 ************************************************************      ELGCSIHS
01365  SEARCH-FOR-EXCEPTED-DAYS.                                        ELGCSIHS
01366      IF NOT MENTAL-FOUND                                          ELGCSIHS
01367          PERFORM FIND-MENTAL-DAYS.                                ELGCSIHS
01368      IF NOT DRUG-FOUND                                            ELGCSIHS
01369          PERFORM FIND-DRUG-DAYS.                                  ELGCSIHS
01370      IF NOT ALCOHOL-FOUND                                         ELGCSIHS
01371          PERFORM FIND-ALCOHOL-DAYS.                               ELGCSIHS
01372      IF WS-TRUE-DAY-CNT > ZERO                                    ELGCSIHS
01373          PERFORM DISPLAY-ANY-TRUE-DAYS-NOT-DISP.                  ELGCSIHS
01374                                                                   ELGCSIHS
01375 ************************************************************      ELGCSIHS
01376 *                                                          *      ELGCSIHS
01377 *        FIND MENTAL DAYS                                  *      ELGCSIHS
01378 *                                                          *      ELGCSIHS
01379 ************************************************************      ELGCSIHS
01380  FIND-MENTAL-DAYS.                                                ELGCSIHS
01381      SET MENTAL-NOT-FOUND TO TRUE.                                ELGCSIHS
01382      IF CSAC-ABM-BP-TBL-PTR NOT = NULL                            ELGCSIHS
01383          PERFORM CHECK-BENEFIT-PROVISION-LEVELX.                  ELGCSIHS
01384      IF MENTAL-NOT-FOUND AND                                      ELGCSIHS
01385                CSAC-ABM-GC-TBL-PTR NOT = NULL                     ELGCSIHS
01386          PERFORM CHECK-CONTRACT-LEVEL-ABM.                        ELGCSIHS
01387      IF MENTAL-NOT-FOUND AND                                      ELGCSIHS
01388                CSAC-ABM-GC-TBL-PTR NOT = NULL                     ELGCSIHS
01389          PERFORM CHECK-GROUP-SPECIFIC-LEVEL-ABM.                  ELGCSIHS
01390                                                                   ELGCSIHS
01391 ************************************************************      ELGCSIHS
01392 *                                                          *      ELGCSIHS
01393 *        INSERT MENTAL DAYS                                *      ELGCSIHS
01394 *                                                          *      ELGCSIHS
01395 ************************************************************      ELGCSIHS
01396  INSERT-MENTAL-DAYS.                                              ELGCSIHS
01397      INITIALIZE WS-COMBO-CNT.                                     ELGCSIHS
01398      IF (NOT DRUG) AND (NOT ALCOHOL)                              ELGCSIHS
01399          PERFORM HOLD-TRUE-MENTAL-DAYS                            ELGCSIHS
01400      ELSE IF DRUG AND ALCOHOL                                     ELGCSIHS
01401          PERFORM INSERT-ALL-THREE-COMBINED                        ELGCSIHS
01402      ELSE IF DRUG                                                 ELGCSIHS
01403          PERFORM INSERT-MENTAL-AND-DRUG-COMBO                     ELGCSIHS
01404      ELSE IF ALCOHOL                                              ELGCSIHS
01405          PERFORM INSERT-MENTAL-AND-ALCOHOL-COMB.                  ELGCSIHS
01406      IF WS-COMBO-CNT > +0                                         ELGCSIHS
01407          PERFORM DISPLAY-DAYS-THAT-ARE-COMBINED.                  ELGCSIHS
01408                                                                   ELGCSIHS
01409 ************************************************************      ELGCSIHS
01410 *                                                          *      ELGCSIHS
01411 *        HOLD TRUE MENTAL DAYS                             *      ELGCSIHS
01412 *                                                          *      ELGCSIHS
01413 ************************************************************      ELGCSIHS
01414  HOLD-TRUE-MENTAL-DAYS.                                           ELGCSIHS
01415      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX) TO                        ELGCSIHS
01416          WS-MENTAL-DAYS.                                          ELGCSIHS
01417      ADD +1                                TO WS-TRUE-DAY-CNT.    ELGCSIHS
01418      SET MENTAL-FOUND                      TO TRUE.               ELGCSIHS
01419                                                                   ELGCSIHS
01420 ************************************************************      ELGCSIHS
01421 *                                                          *      ELGCSIHS
01422 *        INSERT MENTAL AND DRUG COMBO                      *      ELGCSIHS
01423 *                                                          *      ELGCSIHS
01424 ************************************************************      ELGCSIHS
01425  INSERT-MENTAL-AND-DRUG-COMBO.                                    ELGCSIHS
01426      MOVE PC-MENT-DRUG                     TO WS-INFO.            ELGCSIHS
01427      ADD +1                                TO WS-COMBO-CNT.       ELGCSIHS
01428      SET DRUG-FOUND                        TO TRUE.               ELGCSIHS
01429      SET MENTAL-FOUND                      TO TRUE.               ELGCSIHS
01430                                                                   ELGCSIHS
01431 ************************************************************      ELGCSIHS
01432 *                                                          *      ELGCSIHS
01433 *        INSERT MENTAL AND ALCOHOL COMBO                   *      ELGCSIHS
01434 *                                                          *      ELGCSIHS
01435 ************************************************************      ELGCSIHS
01436  INSERT-MENTAL-AND-ALCOHOL-COMB.                                  ELGCSIHS
01437      MOVE PC-MENT-ALC                      TO WS-INFO.            ELGCSIHS
01438      ADD +1                                TO WS-COMBO-CNT.       ELGCSIHS
01439      SET ALCOHOL-FOUND                     TO TRUE.               ELGCSIHS
01440      SET MENTAL-FOUND                      TO TRUE.               ELGCSIHS
01441                                                                   ELGCSIHS
01442 ************************************************************      ELGCSIHS
01443 *                                                          *      ELGCSIHS
01444 *        INSERT ALL THREE COMBINED                         *      ELGCSIHS
01445 *                                                          *      ELGCSIHS
01446 ************************************************************      ELGCSIHS
01447  INSERT-ALL-THREE-COMBINED.                                       ELGCSIHS
01448      MOVE PC-MENT-DRUG-ALC                 TO WS-INFO.            ELGCSIHS
01449      ADD +1                                TO WS-COMBO-CNT.       ELGCSIHS
01450      SET ALCOHOL-FOUND                     TO TRUE.               ELGCSIHS
01451      SET DRUG-FOUND                        TO TRUE.               ELGCSIHS
01452      SET MENTAL-FOUND                      TO TRUE.               ELGCSIHS
01453                                                                   ELGCSIHS
01454 ************************************************************      ELGCSIHS
01455 *                                                          *      ELGCSIHS
01456 *        DISPLAY DAYS THAT ARE COMBINED                    *      ELGCSIHS
01457 *                                                          *      ELGCSIHS
01458 ************************************************************      ELGCSIHS
01459  DISPLAY-DAYS-THAT-ARE-COMBINED.                                  ELGCSIHS
01460      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX) TO WS-DAYS-VALUE.         ELGCSIHS
01461      MOVE PC-COMBINED                      TO WS-INDICATOR.       ELGCSIHS
01462      PERFORM COMPRESS-AND-DISPLAY-OUTPUT-FO.                      ELGCSIHS
01463                                                                   ELGCSIHS
01464 ************************************************************      ELGCSIHS
01465 *                                                          *      ELGCSIHS
01466 *        FIND DRUG DAYS                                    *      ELGCSIHS
01467 *                                                          *      ELGCSIHS
01468 ************************************************************      ELGCSIHS
01469  FIND-DRUG-DAYS.                                                  ELGCSIHS
01470      SET DRUG-NOT-FOUND TO TRUE.                                  ELGCSIHS
01471      IF CSAC-ABM-BP-TBL-PTR NOT = NULL                            ELGCSIHS
01472          PERFORM CHECK-BENEFIT-PROVISION-LEVELX.                  ELGCSIHS
01473      IF DRUG-NOT-FOUND AND                                        ELGCSIHS
01474                CSAC-ABM-GC-TBL-PTR NOT = NULL                     ELGCSIHS
01475          PERFORM CHECK-CONTRACT-LEVEL-ABM.                        ELGCSIHS
01476      IF DRUG-NOT-FOUND AND                                        ELGCSIHS
01477                CSAC-ABM-GC-TBL-PTR NOT = NULL                     ELGCSIHS
01478          PERFORM CHECK-GROUP-SPECIFIC-LEVEL-ABM.                  ELGCSIHS
01479                                                                   ELGCSIHS
01480 ************************************************************      ELGCSIHS
01481 *                                                          *      ELGCSIHS
01482 *        INSERT DRUG DAYS                                  *      ELGCSIHS
01483 *                                                          *      ELGCSIHS
01484 ************************************************************      ELGCSIHS
01485  INSERT-DRUG-DAYS.                                                ELGCSIHS
01486      INITIALIZE WS-COMBO-CNT.                                     ELGCSIHS
01487      IF (NOT MENTAL) AND (NOT ALCOHOL)                            ELGCSIHS
01488          PERFORM HOLD-TRUE-DRUG-DAYS                              ELGCSIHS
01489      ELSE IF MENTAL AND ALCOHOL                                   ELGCSIHS
01490          PERFORM INSERT-ALL-THREE-COMBINED                        ELGCSIHS
01491      ELSE IF MENTAL                                               ELGCSIHS
01492          PERFORM INSERT-MENTAL-AND-DRUG-COMBO                     ELGCSIHS
01493      ELSE IF ALCOHOL                                              ELGCSIHS
01494          PERFORM INSERT-DRUG-AND-ALCOHOL-COMBO.                   ELGCSIHS
01495      IF WS-COMBO-CNT > +0                                         ELGCSIHS
01496          PERFORM DISPLAY-DAYS-THAT-ARE-COMBINED.                  ELGCSIHS
01497                                                                   ELGCSIHS
01498 ************************************************************      ELGCSIHS
01499 *                                                          *      ELGCSIHS
01500 *        HOLD TRUE DRUG DAYS                               *      ELGCSIHS
01501 *                                                          *      ELGCSIHS
01502 ************************************************************      ELGCSIHS
01503  HOLD-TRUE-DRUG-DAYS.                                             ELGCSIHS
01504      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX) TO                        ELGCSIHS
01505          WS-DRUG-DAYS.                                            ELGCSIHS
01506      ADD +1                                TO WS-TRUE-DAY-CNT.    ELGCSIHS
01507      SET DRUG-FOUND                        TO TRUE.               ELGCSIHS
01508                                                                   ELGCSIHS
01509 ************************************************************      ELGCSIHS
01510 *                                                          *      ELGCSIHS
01511 *        INSERT DRUG AND ALCOHOL COMBO                     *      ELGCSIHS
01512 *                                                          *      ELGCSIHS
01513 ************************************************************      ELGCSIHS
01514  INSERT-DRUG-AND-ALCOHOL-COMBO.                                   ELGCSIHS
01515      MOVE PC-DRUG-ALC                      TO WS-INFO.            ELGCSIHS
01516      ADD +1                                TO WS-COMBO-CNT.       ELGCSIHS
01517      SET ALCOHOL-FOUND                     TO TRUE.               ELGCSIHS
01518      SET DRUG-FOUND                        TO TRUE.               ELGCSIHS
01519                                                                   ELGCSIHS
01520 ************************************************************      ELGCSIHS
01521 *                                                          *      ELGCSIHS
01522 *        FIND ALCOHOL DAYS                                 *      ELGCSIHS
01523 *                                                          *      ELGCSIHS
01524 ************************************************************      ELGCSIHS
01525  FIND-ALCOHOL-DAYS.                                               ELGCSIHS
01526      SET ALCOHOL-NOT-FOUND TO TRUE.                               ELGCSIHS
01527      IF CSAC-ABM-BP-TBL-PTR NOT = NULL                            ELGCSIHS
01528          PERFORM CHECK-BENEFIT-PROVISION-LEVELX.                  ELGCSIHS
01529      IF ALCOHOL-NOT-FOUND AND                                     ELGCSIHS
01530                CSAC-ABM-GC-TBL-PTR NOT = NULL                     ELGCSIHS
01531          PERFORM CHECK-CONTRACT-LEVEL-ABM.                        ELGCSIHS
01532      IF ALCOHOL-NOT-FOUND AND                                     ELGCSIHS
01533                CSAC-ABM-GC-TBL-PTR NOT = NULL                     ELGCSIHS
01534          PERFORM CHECK-GROUP-SPECIFIC-LEVEL-ABM.                  ELGCSIHS
01535                                                                   ELGCSIHS
01536 ************************************************************      ELGCSIHS
01537 *                                                          *      ELGCSIHS
01538 *        INSERT ALCOHOL DAYS                               *      ELGCSIHS
01539 *                                                          *      ELGCSIHS
01540 ************************************************************      ELGCSIHS
01541  INSERT-ALCOHOL-DAYS.                                             ELGCSIHS
01542      IF NOT MENTAL AND NOT DRUG                                   ELGCSIHS
01543          PERFORM HOLD-TRUE-ALCOHOL-DAYS                           ELGCSIHS
01544      ELSE IF MENTAL AND DRUG                                      ELGCSIHS
01545          PERFORM INSERT-ALL-THREE-COMBINED                        ELGCSIHS
01546      ELSE IF DRUG                                                 ELGCSIHS
01547          PERFORM INSERT-DRUG-AND-ALCOHOL-COMBO                    ELGCSIHS
01548      ELSE IF MENTAL                                               ELGCSIHS
01549          PERFORM INSERT-MENTAL-AND-ALCOHOL-COMB.                  ELGCSIHS
01550      IF WS-COMBO-CNT > +0                                         ELGCSIHS
01551          PERFORM DISPLAY-DAYS-THAT-ARE-COMBINED.                  ELGCSIHS
01552                                                                   ELGCSIHS
01553 ************************************************************      ELGCSIHS
01554 *                                                          *      ELGCSIHS
01555 *        HOLD TRUE ALCOHOL DAYS                            *      ELGCSIHS
01556 *                                                          *      ELGCSIHS
01557 ************************************************************      ELGCSIHS
01558  HOLD-TRUE-ALCOHOL-DAYS.                                          ELGCSIHS
01559      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX) TO                        ELGCSIHS
01560          WS-ALCOHOL-DAYS.                                         ELGCSIHS
01561      ADD +1                                TO WS-TRUE-DAY-CNT.    ELGCSIHS
01562      SET ALCOHOL-FOUND                     TO TRUE.               ELGCSIHS
01563                                                                   ELGCSIHS
01564 ************************************************************      ELGCSIHS
01565 *                                                          *      ELGCSIHS
01566 *        COMPRESS AND DISPLAY OUTPUT FOR EXCEPTION DAYS    *      ELGCSIHS
01567 *                                                          *      ELGCSIHS
01568 ************************************************************      ELGCSIHS
01569  COMPRESS-AND-DISPLAY-OUTPUT-FO.                                  ELGCSIHS
01570      IF UNLIMITED-AMT                                             ELGCSIHS
01571          PERFORM INDICATE-UNLIMITED-DAYS.                         ELGCSIHS
01572      ADD +1                          TO TCAR-FROM-SUB.            ELGCSIHS
01573      MOVE WS-AREA-FOR-DAYS           TO TCAR-FROM-LINE            ELGCSIHS
01574          (TCAR-FROM-SUB).                                         ELGCSIHS
01575      PERFORM CALL-TEXT-COMPRESSION-MODULE.                        ELGCSIHS
01576      PERFORM SETUP-FOR-UNSTRING-OPERATION.                        ELGCSIHS
01577      PERFORM CALL-TEXT-UNSTRING-MODULE.                           ELGCSIHS
01578      MOVE PC-LEFT-SIDE-BAR-ONLY      TO WS-TITLE.                 ELGCSIHS
01579      PERFORM MOVE-FORMATTED-TEXT-TO-OUTPUTX                       ELGCSIHS
01580          VARYING TCAR-X FROM +1 BY +1                             ELGCSIHS
01581                 UNTIL   TCAR-X > TCAR-OUTPUT-FIELDS-USED.         ELGCSIHS
01582      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSIHS
01583      PERFORM INITIALIZE-TEXT-COMPRESSION-AR.                      ELGCSIHS
01584                                                                   ELGCSIHS
01585 ************************************************************      ELGCSIHS
01586 *                                                          *      ELGCSIHS
01587 *        INDICATE UNLIMITED DAYS                           *      ELGCSIHS
01588 *                                                          *      ELGCSIHS
01589 ************************************************************      ELGCSIHS
01590  INDICATE-UNLIMITED-DAYS.                                         ELGCSIHS
01591      MOVE PC-UNLIMITED               TO                           ELGCSIHS
01592          WS-DAYS-UNLIMITED-CK.                                    ELGCSIHS
01593                                                                   ELGCSIHS
01594 ************************************************************      ELGCSIHS
01595 *                                                          *      ELGCSIHS
01596 *        INITIALIZE TEXT COMPRESSION AREA                  *      ELGCSIHS
01597 *                                                          *      ELGCSIHS
01598 ************************************************************      ELGCSIHS
01599  INITIALIZE-TEXT-COMPRESSION-AR.                                  ELGCSIHS
01600      INITIALIZE TCAR-FROM-SUB                                     ELGCSIHS
01601                 TCAR-FROM-LENGTH                                  ELGCSIHS
01602                 TCAR-FROM-AREA.                                   ELGCSIHS
01603                                                                   ELGCSIHS
01604 ************************************************************      ELGCSIHS
01605 *                                                          *      ELGCSIHS
01606 *        CALL TEXT COMPRESSION MODULE                      *      ELGCSIHS
01607 *                                                          *      ELGCSIHS
01608 ************************************************************      ELGCSIHS
01609  CALL-TEXT-COMPRESSION-MODULE.                                    ELGCSIHS
01610      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGCSIHS
01611                                                                   ELGCSIHS
01612 ************************************************************      ELGCSIHS
01613 *                                                          *      ELGCSIHS
01614 *        SETUP FOR UNSTRING OPERATION                      *      ELGCSIHS
01615 *                                                          *      ELGCSIHS
01616 ************************************************************      ELGCSIHS
01617  SETUP-FOR-UNSTRING-OPERATION.                                    ELGCSIHS
01618      MOVE +10 TO TCAR-OUTPUT-FIELD-COUNT.                         ELGCSIHS
01619      MOVE +46 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELGCSIHS
01620      MOVE +46 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELGCSIHS
01621      MOVE +46 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELGCSIHS
01622      MOVE +46 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELGCSIHS
01623      MOVE +46 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELGCSIHS
01624      MOVE +46 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELGCSIHS
01625      MOVE +46 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELGCSIHS
01626      MOVE +46 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELGCSIHS
01627      MOVE +46 TO TCAR-OUTPUT-FIELD-9-LEN.                         ELGCSIHS
01628      MOVE +46 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELGCSIHS
01629                                                                   ELGCSIHS
01630 ************************************************************      ELGCSIHS
01631 *                                                          *      ELGCSIHS
01632 *        CALL TEXT UNSTRING MODULE                         *      ELGCSIHS
01633 *                                                          *      ELGCSIHS
01634 ************************************************************      ELGCSIHS
01635  CALL-TEXT-UNSTRING-MODULE.                                       ELGCSIHS
01636      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGCSIHS
01637                                                                   ELGCSIHS
01638 ************************************************************      ELGCSIHS
01639 *                                                          *      ELGCSIHS
01640 *        MOVE FORMATTED TEXT TO OUTPUT AREA                *      ELGCSIHS
01641 *                                                          *      ELGCSIHS
01642 ************************************************************      ELGCSIHS
01643  MOVE-FORMATTED-TEXT-TO-OUTPUTX.                                  ELGCSIHS
01644      IF TCAR-X > 1                                                ELGCSIHS
01645          PERFORM INSERT-LEFT-SIDE-WITH-BAR-ONLY.                  ELGCSIHS
01646      INITIALIZE WS-SENTENCE.                                      ELGCSIHS
01647      MOVE TCAR-OPF-DATA (TCAR-X)  TO WS-SENTENCE.                 ELGCSIHS
01648      ADD +1                       TO COF-NBR-DTL-LINES.           ELGCSIHS
01649      MOVE WS-HOLD-LINE            TO COF-DTL-LINE                 ELGCSIHS
01650          (COF-NBR-DTL-LINES).                                     ELGCSIHS
01651                                                                   ELGCSIHS
01652 ************************************************************      ELGCSIHS
01653 *                                                          *      ELGCSIHS
01654 *        INSERT LEFT SIDE WITH BAR ONLY                    *      ELGCSIHS
01655 *                                                          *      ELGCSIHS
01656 ************************************************************      ELGCSIHS
01657  INSERT-LEFT-SIDE-WITH-BAR-ONLY.                                  ELGCSIHS
01658      MOVE PC-LEFT-SIDE-BAR-ONLY    TO WS-TITLE.                   ELGCSIHS
01659                                                                   ELGCSIHS
01660 ************************************************************      ELGCSIHS
01661 *                                                          *      ELGCSIHS
01662 *        CREATE BENEFIT PERIOD LINE                        *      ELGCSIHS
01663 *                                                          *      ELGCSIHS
01664 ************************************************************      ELGCSIHS
01665  CREATE-BENEFIT-PERIOD-LINE.                                      ELGCSIHS
01666      PERFORM TRANSLATE-ABM-BENEFIT-PERIOD.                        ELGCSIHS
01667      ADD +1                        TO TCAR-FROM-SUB.              ELGCSIHS
01668      MOVE PC-PER                   TO TCAR-FROM-LINE              ELGCSIHS
01669          (TCAR-FROM-SUB).                                         ELGCSIHS
01670      PERFORM COMPRESS-AND-UNSTRING-TRANSLAT.                      ELGCSIHS
01671      ADD +1                        TO COF-NBR-DTL-LINES.          ELGCSIHS
01672      STRING WS-BP-PHRASE          DELIMITED BY SIZE               ELGCSIHS
01673             TCAR-OPF-DATA (1)     DELIMITED BY SIZE               ELGCSIHS
01674             INTO COF-DTL-LINE (1).                                ELGCSIHS
01675      PERFORM MOVE-FORMATTED-TEXT-TO-OUTPUTX                       ELGCSIHS
01676          VARYING TCAR-X FROM +2 BY +1                             ELGCSIHS
01677                 UNTIL   TCAR-X > TCAR-OUTPUT-FIELDS-USED.         ELGCSIHS
01678      ADD +1                        TO                             ELGCSIHS
01679          COF-NBR-DTL-LINES.                                       ELGCSIHS
01680      MOVE PC-LEFT-SIDE-BAR-ONLY    TO COF-DTL-LINE                ELGCSIHS
01681          (COF-NBR-DTL-LINES).                                     ELGCSIHS
01682      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSIHS
01683      PERFORM INITIALIZE-TEXT-COMPRESSION-AR.                      ELGCSIHS
01684                                                                   ELGCSIHS
01685 ************************************************************      ELGCSIHS
01686 *                                                          *      ELGCSIHS
01687 *        TRANSLATE ABM BENEFIT PERIOD                      *      ELGCSIHS
01688 *                                                          *      ELGCSIHS
01689 ************************************************************      ELGCSIHS
01690  TRANSLATE-ABM-BENEFIT-PERIOD.                                    ELGCSIHS
01691      MOVE PC-ABM                TO CMF-RECORD-PREFIX.             ELGCSIHS
01692      MOVE 'BAMA-BENEFIT-PERIOD' TO CMF-ELEMENT-SYSTEM-NAME.       ELGCSIHS
01693      MOVE WS-BENEFIT-PERIOD     TO CMF-CODE-VALUE.                ELGCSIHS
01694      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELGCSIHS
01695                                                                   ELGCSIHS
01696                                                                   ELGCSIHS
01697 ************************************************************      ELGCSIHS
01698 *                                                          *      ELGCSIHS
01699 *        CALL CODES MANUAL INTERFACE                       *      ELGCSIHS
01700 *                                                          *      ELGCSIHS
01701 ************************************************************      ELGCSIHS
01702  CALL-CODES-MANUAL-INTERFACE.                                     ELGCSIHS
01703      EXEC CICS LINK                                               ELGCSIHS
01704                PROGRAM ('ELUCMIF')                                ELGCSIHS
01705                COMMAREA (DFHCOMMAREA)                             ELGCSIHS
01706         END-EXEC.                                                 ELGCSIHS
01707      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGCSIHS
01708      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSIHS
01709          ADDRESS OF CMF-DESCR.                                    ELGCSIHS
01710                                                                   ELGCSIHS
01711 ************************************************************      ELGCSIHS
01712 *                                                          *      ELGCSIHS
01713 *        COMPRESS AND UNSTRING TRANSLATION                 *      ELGCSIHS
01714 *                                                          *      ELGCSIHS
01715 ************************************************************      ELGCSIHS
01716  COMPRESS-AND-UNSTRING-TRANSLAT.                                  ELGCSIHS
01717      PERFORM MOVE-TRANSLATION-INTO-COMPRESS                       ELGCSIHS
01718          VARYING CMF-DESCR-IDX FROM +1 BY +1                      ELGCSIHS
01719                 UNTIL   CMF-DESCR-IDX >                           ELGCSIHS
01720              CMF-NBR-DESCR-LINES.                                 ELGCSIHS
01721      PERFORM CALL-TEXT-COMPRESSION-MODULE.                        ELGCSIHS
01722      PERFORM SETUP-FOR-UNSTRING-OPERATION.                        ELGCSIHS
01723      PERFORM CALL-TEXT-UNSTRING-MODULE.                           ELGCSIHS
01724                                                                   ELGCSIHS
01725 ************************************************************      ELGCSIHS
01726 *                                                          *      ELGCSIHS
01727 *        MOVE TRANSLATION INTO COMPRESSION AREA            *      ELGCSIHS
01728 *                                                          *      ELGCSIHS
01729 ************************************************************      ELGCSIHS
01730  MOVE-TRANSLATION-INTO-COMPRESS.                                  ELGCSIHS
01731      ADD +1              TO TCAR-FROM-SUB.                        ELGCSIHS
01732      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                          ELGCSIHS
01733                          TO TCAR-FROM-LINE                        ELGCSIHS
01734          (TCAR-FROM-SUB).                                         ELGCSIHS
01735                                                                   ELGCSIHS
01736 ************************************************************      ELGCSIHS
01737 *                                                          *      ELGCSIHS
01738 *        CREATE INTERVAL LINE                              *      ELGCSIHS
01739 *                                                          *      ELGCSIHS
01740 ************************************************************      ELGCSIHS
01741  CREATE-INTERVAL-LINE.                                            ELGCSIHS
01742      PERFORM TRANSLATE-ABM-INTERVAL-TYPE.                         ELGCSIHS
01743      ADD +1                        TO TCAR-FROM-SUB.              ELGCSIHS
01744      MOVE WS-INTERVAL-TIME-FACTOR  TO TCAR-FROM-LINE              ELGCSIHS
01745          (TCAR-FROM-SUB).                                         ELGCSIHS
01746      PERFORM COMPRESS-AND-UNSTRING-TRANSLAT.                      ELGCSIHS
01747      ADD +1                        TO COF-NBR-DTL-LINES.          ELGCSIHS
01748      STRING WS-INTERVAL-PHRASE    DELIMITED BY SIZE               ELGCSIHS
01749             TCAR-OPF-DATA (1)     DELIMITED BY SIZE               ELGCSIHS
01750             INTO COF-DTL-LINE (1).                                ELGCSIHS
01751      PERFORM MOVE-FORMATTED-TEXT-TO-OUTPUTX                       ELGCSIHS
01752          VARYING TCAR-X FROM +2 BY +1                             ELGCSIHS
01753                 UNTIL   TCAR-X > TCAR-OUTPUT-FIELDS-USED.         ELGCSIHS
01754      ADD +1                        TO                             ELGCSIHS
01755          COF-NBR-DTL-LINES.                                       ELGCSIHS
01756      MOVE PC-LEFT-SIDE-BAR-ONLY    TO COF-DTL-LINE                ELGCSIHS
01757          (COF-NBR-DTL-LINES).                                     ELGCSIHS
01758      PERFORM CALL-OUTPUT-INTERFACE.                               ELGCSIHS
01759      PERFORM INITIALIZE-TEXT-COMPRESSION-AR.                      ELGCSIHS
01760                                                                   ELGCSIHS
01761 ************************************************************      ELGCSIHS
01762 *                                                          *      ELGCSIHS
01763 *        TRANSLATE ABM INTERVAL TYPE                       *      ELGCSIHS
01764 *                                                          *      ELGCSIHS
01765 ************************************************************      ELGCSIHS
01766  TRANSLATE-ABM-INTERVAL-TYPE.                                     ELGCSIHS
01767      MOVE PC-ABM                   TO CMF-RECORD-PREFIX.          ELGCSIHS
01768      MOVE 'BAMA-INTERVAL-TYPE'     TO CMF-ELEMENT-SYSTEM-NAME.    ELGCSIHS
01769      MOVE WS-INTERVAL-TYPE         TO CMF-CODE-VALUE.             ELGCSIHS
01770      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELGCSIHS
01771                                                                   ELGCSIHS
01772 ************************************************************      ELGCSIHS
01773 *                                                          *      ELGCSIHS
01774 *        CALL SENTENCE INTERFACE                           *      ELGCSIHS
01775 *                                                          *      ELGCSIHS
01776 ************************************************************      ELGCSIHS
01777  CALL-SENTENCE-INTERFACE.                                         ELGCSIHS
01778      EXEC CICS LINK                                               ELGCSIHS
01779                PROGRAM ('ELUCSENT')                               ELGCSIHS
01780                COMMAREA (DFHCOMMAREA)                             ELGCSIHS
01781         END-EXEC.                                                 ELGCSIHS
01782                                                                   ELGCSIHS
01783 ************************************************************      ELGCSIHS
01784 *                                                          *      ELGCSIHS
01785 *        SEARCH FOR DUMMY ANCILLARY PROVISION              *      ELGCSIHS
01786 *                                                          *      ELGCSIHS
01787 ************************************************************      ELGCSIHS
01788  SEARCH-FOR-DUMMY-ANCILLARY-PRO.                                  ELGCSIHS
01789      INITIALIZE WS-PROVISION-SW.                                  ELGCSIHS
01790      PERFORM FIND-OCCURENCE-OF-ANC-IN-BP-MA                       ELGCSIHS
01791          VARYING CSBP-X-IDX FROM +1 BY +1                         ELGCSIHS
01792                 UNTIL   CSBP-X-IDX > CSBP-TBL-CNT                 ELGCSIHS
01793                 OR      PROVISION-FOUND.                          ELGCSIHS
01794                                                                   ELGCSIHS
01795 ************************************************************      ELGCSIHS
01796 *                                                          *      ELGCSIHS
01797 *        FIND OCCURENCE OF ANC IN BP MATRIX                *      ELGCSIHS
01798 *                                                          *      ELGCSIHS
01799 ************************************************************      ELGCSIHS
01800  FIND-OCCURENCE-OF-ANC-IN-BP-MA.                                  ELGCSIHS
01801      IF CSBP-BP-KEY (CSBP-X-IDX) = PC-ANC                         ELGCSIHS
01802          PERFORM DETERMINE-ANC-COVERAGE.                          ELGCSIHS
01803                                                                   ELGCSIHS
01804                                                                   ELGCSIHS
01805 ************************************************************      ELGCSIHS
01806 *                                                          *      ELGCSIHS
01807 *        DETERMINE ANC COVERAGE                            *      ELGCSIHS
01808 *                                                          *      ELGCSIHS
01809 ************************************************************      ELGCSIHS
01810  DETERMINE-ANC-COVERAGE.                                          ELGCSIHS
01811      IF CSBP-COVERED (CSBP-X-IDX)                                 ELGCSIHS
01812          PERFORM INVESTIGATE-ALL-THE-OTHER-ANCI.                  ELGCSIHS
01813      SET PROVISION-FOUND TO TRUE.                                 ELGCSIHS
01814                                                                   ELGCSIHS
01815                                                                   ELGCSIHS
01816 ************************************************************      ELGCSIHS
01817 *                                                          *      ELGCSIHS
01818 *        INVESTIGATE ALL THE OTHER ANCILLARY PROVISIONS    *      ELGCSIHS
01819 *                                                          *      ELGCSIHS
01820 ************************************************************      ELGCSIHS
01821  INVESTIGATE-ALL-THE-OTHER-ANCI.                                  ELGCSIHS
01822      MOVE CSBP-PAYMENT-LVL (CSBP-X-IDX) TO                        ELGCSIHS
01823          WS-ANC-PAYMENT-LVL.                                      ELGCSIHS
01824      SET CSBP-X-IDX UP BY +1.                                     ELGCSIHS
01825      PERFORM CHECK-FOR-MATCHING-PAYMENT-LEV                       ELGCSIHS
01826          VARYING CSBP-X-IDX FROM CSBP-X-IDX BY +1                 ELGCSIHS
01827                 UNTIL   CSBP-X-IDX > CSBP-TBL-CNT.                ELGCSIHS
01828                                                                   ELGCSIHS
01829                                                                   ELGCSIHS
01830 ************************************************************      ELGCSIHS
01831 *                                                          *      ELGCSIHS
01832 *        CHECK FOR MATCHING PAYMENT LEVELS WITH DUMMY ANCIL*      ELGCSIHS
01833 *                                                          *      ELGCSIHS
01834 ************************************************************      ELGCSIHS
01835  CHECK-FOR-MATCHING-PAYMENT-LEV.                                  ELGCSIHS
01836      IF CSBP-PAYMENT-LVL (CSBP-X-IDX) =                           ELGCSIHS
01837          WS-ANC-PAYMENT-LVL                                       ELGCSIHS
01838          PERFORM INITIALIZE-MATCHING-PAYMENT-LE.                  ELGCSIHS
01839                                                                   ELGCSIHS
01840 ************************************************************      ELGCSIHS
01841 *                                                          *      ELGCSIHS
01842 *        INITIALIZE MATCHING PAYMENT LEVELS WITH DUMMY ANCI*      ELGCSIHS
01843 *                                                          *      ELGCSIHS
01844 ************************************************************      ELGCSIHS
01845  INITIALIZE-MATCHING-PAYMENT-LE.                                  ELGCSIHS
01846      MOVE ZEROS TO CSBP-PAYMENT-LVL (CSBP-X-IDX).                 ELGCSIHS
01847                                                                   ELGCSIHS
01848 ************************************************************      ELGCSIHS
01849 *                                                          *      ELGCSIHS
01850 *        CALL CONTRACT SUMMARY OUTPUT INTERFACE            *      ELGCSIHS
01851 *                                                          *      ELGCSIHS
01852 ************************************************************      ELGCSIHS
01853  CALL-CONTRACT-SUMMARY-OUTPUT-I.                                  ELGCSIHS
01854      EXEC CICS LINK                                               ELGCSIHS
01855                PROGRAM ('ELUCSOUT')                               ELGCSIHS
01856                COMMAREA (DFHCOMMAREA)                             ELGCSIHS
01857         END-EXEC.                                                 ELGCSIHS
01858                                                                   ELGCSIHS
01859 ************************************************************      ELGCSIHS
01860 *                                                          *      ELGCSIHS
01861 *        DISPLAY ANY TRUE DAYS NOT DISPLAYED               *      ELGCSIHS
01862 *                                                          *      ELGCSIHS
01863 ************************************************************      ELGCSIHS
01864  DISPLAY-ANY-TRUE-DAYS-NOT-DISP.                                  ELGCSIHS
01865      IF WS-TRUE-DAY-CNT > +2                                      ELGCSIHS
01866          PERFORM DISPLAY-ALL-THREE-TRUE-DAYS                      ELGCSIHS
01867      ELSE IF WS-TRUE-DAY-CNT > +1                                 ELGCSIHS
01868          PERFORM DISPLAY-TWO-TRUE-DAYS                            ELGCSIHS
01869      ELSE                                                         ELGCSIHS
01870          PERFORM DISPLAY-ONLY-ONE-TYPE-OF-TRUEX.                  ELGCSIHS
01871                                                                   ELGCSIHS
01872 ************************************************************      ELGCSIHS
01873 *                                                          *      ELGCSIHS
01874 *        DISPLAY ALL THREE TRUE DAYS                       *      ELGCSIHS
01875 *                                                          *      ELGCSIHS
01876 ************************************************************      ELGCSIHS
01877  DISPLAY-ALL-THREE-TRUE-DAYS.                                     ELGCSIHS
01878      IF WS-MENTAL-DAYS = WS-DRUG-DAYS AND                         ELGCSIHS
01879                WS-DRUG-DAYS   = WS-ALCOHOL-DAYS                   ELGCSIHS
01880          PERFORM DISPLAY-ALL-THREE-ARE-SAME                       ELGCSIHS
01881      ELSE IF WS-MENTAL-DAYS = WS-DRUG-DAYS                        ELGCSIHS
01882          PERFORM DISPLAY-MENTAL-AND-DRUG-TOGETH                   ELGCSIHS
01883      ELSE IF WS-MENTAL-DAYS = WS-ALCOHOL-DAYS                     ELGCSIHS
01884          PERFORM DISPLAY-MENTAL-AND-ALCOHOL-TOG                   ELGCSIHS
01885      ELSE IF WS-DRUG-DAYS   = WS-ALCOHOL-DAYS                     ELGCSIHS
01886          PERFORM DISPLAY-DRUG-AND-ALCOHOL-TOGET                   ELGCSIHS
01887      ELSE                                                         ELGCSIHS
01888          PERFORM DISPLAY-ALL-THREE-SEPARATE.                      ELGCSIHS
01889                                                                   ELGCSIHS
01890 ************************************************************      ELGCSIHS
01891 *                                                          *      ELGCSIHS
01892 *        DISPLAY ALL THREE ARE SAME                        *      ELGCSIHS
01893 *                                                          *      ELGCSIHS
01894 ************************************************************      ELGCSIHS
01895  DISPLAY-ALL-THREE-ARE-SAME.                                      ELGCSIHS
01896      MOVE PC-MENT-DRUG-ALC           TO WS-INFO.                  ELGCSIHS
01897      MOVE WS-MENTAL-DAYS             TO WS-DAYS-VALUE.            ELGCSIHS
01898      MOVE PC-EACH                    TO WS-INDICATOR.             ELGCSIHS
01899      PERFORM COMPRESS-AND-DISPLAY-OUTPUT-FO.                      ELGCSIHS
01900                                                                   ELGCSIHS
01901 ************************************************************      ELGCSIHS
01902 *                                                          *      ELGCSIHS
01903 *        DISPLAY MENTAL AND DRUG TOGETHER                  *      ELGCSIHS
01904 *                                                          *      ELGCSIHS
01905 ************************************************************      ELGCSIHS
01906  DISPLAY-MENTAL-AND-DRUG-TOGETH.                                  ELGCSIHS
01907      PERFORM DISPLAY-MENTAL-AND-DRUG-AS-SAM.                      ELGCSIHS
01908      PERFORM DISPLAY-TRUE-ALCOHOL-DAYS.                           ELGCSIHS
01909                                                                   ELGCSIHS
01910 ************************************************************      ELGCSIHS
01911 *                                                          *      ELGCSIHS
01912 *        DISPLAY MENTAL AND ALCOHOL TOGETHER               *      ELGCSIHS
01913 *                                                          *      ELGCSIHS
01914 ************************************************************      ELGCSIHS
01915  DISPLAY-MENTAL-AND-ALCOHOL-TOG.                                  ELGCSIHS
01916      PERFORM DISPLAY-MENTAL-AND-ALCOHOL-MAT.                      ELGCSIHS
01917      PERFORM DISPLAY-TRUE-DRUG-DAYS.                              ELGCSIHS
01918                                                                   ELGCSIHS
01919 ************************************************************      ELGCSIHS
01920 *                                                          *      ELGCSIHS
01921 *        DISPLAY DRUG AND ALCOHOL TOGETHER                 *      ELGCSIHS
01922 *                                                          *      ELGCSIHS
01923 ************************************************************      ELGCSIHS
01924  DISPLAY-DRUG-AND-ALCOHOL-TOGET.                                  ELGCSIHS
01925      PERFORM DISPLAY-DRUG-AND-ALCOHOL-MATCH.                      ELGCSIHS
01926      PERFORM DISPLAY-TRUE-MENTAL-DAYS.                            ELGCSIHS
01927                                                                   ELGCSIHS
01928 ************************************************************      ELGCSIHS
01929 *                                                          *      ELGCSIHS
01930 *        DISPLAY ALL THREE SEPARATE                        *      ELGCSIHS
01931 *                                                          *      ELGCSIHS
01932 ************************************************************      ELGCSIHS
01933  DISPLAY-ALL-THREE-SEPARATE.                                      ELGCSIHS
01934      PERFORM DISPLAY-TRUE-MENTAL-DAYS.                            ELGCSIHS
01935      PERFORM DISPLAY-TRUE-DRUG-DAYS.                              ELGCSIHS
01936      PERFORM DISPLAY-TRUE-ALCOHOL-DAYS.                           ELGCSIHS
01937                                                                   ELGCSIHS
01938 ************************************************************      ELGCSIHS
01939 *                                                          *      ELGCSIHS
01940 *        DISPLAY TWO TRUE DAYS                             *      ELGCSIHS
01941 *                                                          *      ELGCSIHS
01942 ************************************************************      ELGCSIHS
01943  DISPLAY-TWO-TRUE-DAYS.                                           ELGCSIHS
01944      IF (WS-MENTAL-DAYS > ZERO) AND                               ELGCSIHS
01945                (WS-DRUG-DAYS > ZERO)                              ELGCSIHS
01946          PERFORM CHECK-WHETHER-MENTAL-AND-DRUGX                   ELGCSIHS
01947      ELSE IF (WS-MENTAL-DAYS > ZERO) AND                          ELGCSIHS
01948                (WS-ALCOHOL-DAYS > ZERO)                           ELGCSIHS
01949          PERFORM CHECK-WHETHER-MENTAL-AND-ALCOH                   ELGCSIHS
01950      ELSE IF (WS-DRUG-DAYS > ZERO)   AND                          ELGCSIHS
01951                (WS-ALCOHOL-DAYS > ZERO)                           ELGCSIHS
01952          PERFORM CHECK-WHETHER-DRUG-AND-ALCOHOL.                  ELGCSIHS
01953                                                                   ELGCSIHS
01954                                                                   ELGCSIHS
01955 ************************************************************      ELGCSIHS
01956 *                                                          *      ELGCSIHS
01957 *        CHECK WHETHER MENTAL AND DRUG ARE SAME            *      ELGCSIHS
01958 *                                                          *      ELGCSIHS
01959 ************************************************************      ELGCSIHS
01960  CHECK-WHETHER-MENTAL-AND-DRUGX.                                  ELGCSIHS
01961      IF WS-MENTAL-DAYS = WS-DRUG-DAYS                             ELGCSIHS
01962          PERFORM DISPLAY-MENTAL-AND-DRUG-AS-SAM                   ELGCSIHS
01963      ELSE                                                         ELGCSIHS
01964          PERFORM DISPLAY-MENTAL-AND-DRUG-SEPARA.                  ELGCSIHS
01965                                                                   ELGCSIHS
01966 ************************************************************      ELGCSIHS
01967 *                                                          *      ELGCSIHS
01968 *        DISPLAY MENTAL AND DRUG AS SAME                   *      ELGCSIHS
01969 *                                                          *      ELGCSIHS
01970 ************************************************************      ELGCSIHS
01971  DISPLAY-MENTAL-AND-DRUG-AS-SAM.                                  ELGCSIHS
01972      MOVE PC-MENT-DRUG               TO WS-INFO.                  ELGCSIHS
01973      MOVE WS-MENTAL-DAYS             TO WS-DAYS-VALUE.            ELGCSIHS
01974      MOVE PC-EACH                    TO WS-INDICATOR.             ELGCSIHS
01975      PERFORM COMPRESS-AND-DISPLAY-OUTPUT-FO.                      ELGCSIHS
01976                                                                   ELGCSIHS
01977 ************************************************************      ELGCSIHS
01978 *                                                          *      ELGCSIHS
01979 *        DISPLAY MENTAL AND DRUG SEPARATE                  *      ELGCSIHS
01980 *                                                          *      ELGCSIHS
01981 ************************************************************      ELGCSIHS
01982  DISPLAY-MENTAL-AND-DRUG-SEPARA.                                  ELGCSIHS
01983      PERFORM DISPLAY-TRUE-MENTAL-DAYS.                            ELGCSIHS
01984      PERFORM DISPLAY-TRUE-DRUG-DAYS.                              ELGCSIHS
01985                                                                   ELGCSIHS
01986 ************************************************************      ELGCSIHS
01987 *                                                          *      ELGCSIHS
01988 *        CHECK WHETHER MENTAL AND ALCOHOL ARE SAME         *      ELGCSIHS
01989 *                                                          *      ELGCSIHS
01990 ************************************************************      ELGCSIHS
01991  CHECK-WHETHER-MENTAL-AND-ALCOH.                                  ELGCSIHS
01992      IF WS-MENTAL-DAYS = WS-ALCOHOL-DAYS                          ELGCSIHS
01993          PERFORM DISPLAY-MENTAL-AND-ALCOHOL-MAT                   ELGCSIHS
01994      ELSE                                                         ELGCSIHS
01995          PERFORM DISPLAY-MENTAL-AND-ALCOHOL-SEP.                  ELGCSIHS
01996                                                                   ELGCSIHS
01997 ************************************************************      ELGCSIHS
01998 *                                                          *      ELGCSIHS
01999 *        DISPLAY MENTAL AND ALCOHOL MATCH                  *      ELGCSIHS
02000 *                                                          *      ELGCSIHS
02001 ************************************************************      ELGCSIHS
02002  DISPLAY-MENTAL-AND-ALCOHOL-MAT.                                  ELGCSIHS
02003      MOVE PC-MENT-ALC                TO WS-INFO.                  ELGCSIHS
02004      MOVE WS-MENTAL-DAYS             TO WS-DAYS-VALUE.            ELGCSIHS
02005      MOVE PC-EACH                    TO WS-INDICATOR.             ELGCSIHS
02006      PERFORM COMPRESS-AND-DISPLAY-OUTPUT-FO.                      ELGCSIHS
02007                                                                   ELGCSIHS
02008 ************************************************************      ELGCSIHS
02009 *                                                          *      ELGCSIHS
02010 *        DISPLAY MENTAL AND ALCOHOL SEPARATE               *      ELGCSIHS
02011 *                                                          *      ELGCSIHS
02012 ************************************************************      ELGCSIHS
02013  DISPLAY-MENTAL-AND-ALCOHOL-SEP.                                  ELGCSIHS
02014      PERFORM DISPLAY-TRUE-MENTAL-DAYS.                            ELGCSIHS
02015      PERFORM DISPLAY-TRUE-ALCOHOL-DAYS.                           ELGCSIHS
02016                                                                   ELGCSIHS
02017 ************************************************************      ELGCSIHS
02018 *                                                          *      ELGCSIHS
02019 *        CHECK WHETHER DRUG AND ALCOHOL ARE SAME           *      ELGCSIHS
02020 *                                                          *      ELGCSIHS
02021 ************************************************************      ELGCSIHS
02022  CHECK-WHETHER-DRUG-AND-ALCOHOL.                                  ELGCSIHS
02023      IF WS-DRUG-DAYS = WS-ALCOHOL-DAYS                            ELGCSIHS
02024          PERFORM DISPLAY-DRUG-AND-ALCOHOL-MATCH                   ELGCSIHS
02025      ELSE                                                         ELGCSIHS
02026          PERFORM DISPLAY-DRUG-AND-ALCOHOL-SEPAR.                  ELGCSIHS
02027                                                                   ELGCSIHS
02028                                                                   ELGCSIHS
02029 ************************************************************      ELGCSIHS
02030 *                                                          *      ELGCSIHS
02031 *        DISPLAY DRUG AND ALCOHOL MATCH                    *      ELGCSIHS
02032 *                                                          *      ELGCSIHS
02033 ************************************************************      ELGCSIHS
02034  DISPLAY-DRUG-AND-ALCOHOL-MATCH.                                  ELGCSIHS
02035      MOVE PC-DRUG-ALC                TO WS-INFO.                  ELGCSIHS
02036      MOVE WS-DRUG-DAYS               TO WS-DAYS-VALUE.            ELGCSIHS
02037      MOVE PC-EACH                    TO WS-INDICATOR.             ELGCSIHS
02038      PERFORM COMPRESS-AND-DISPLAY-OUTPUT-FO.                      ELGCSIHS
02039                                                                   ELGCSIHS
02040 ************************************************************      ELGCSIHS
02041 *                                                          *      ELGCSIHS
02042 *        DISPLAY DRUG AND ALCOHOL SEPARATE                 *      ELGCSIHS
02043 *                                                          *      ELGCSIHS
02044 ************************************************************      ELGCSIHS
02045  DISPLAY-DRUG-AND-ALCOHOL-SEPAR.                                  ELGCSIHS
02046      PERFORM DISPLAY-TRUE-DRUG-DAYS.                              ELGCSIHS
02047      PERFORM DISPLAY-TRUE-ALCOHOL-DAYS.                           ELGCSIHS
02048                                                                   ELGCSIHS
02049                                                                   ELGCSIHS
02050 ************************************************************      ELGCSIHS
02051 *                                                          *      ELGCSIHS
02052 *        DISPLAY ONLY ONE TYPE OF TRUE DAYS                *      ELGCSIHS
02053 *                                                          *      ELGCSIHS
02054 ************************************************************      ELGCSIHS
02055  DISPLAY-ONLY-ONE-TYPE-OF-TRUEX.                                  ELGCSIHS
02056      IF WS-MENTAL-DAYS > ZERO                                     ELGCSIHS
02057          PERFORM DISPLAY-TRUE-MENTAL-DAYS                         ELGCSIHS
02058      ELSE IF WS-DRUG-DAYS > ZERO                                  ELGCSIHS
02059          PERFORM DISPLAY-TRUE-DRUG-DAYS                           ELGCSIHS
02060      ELSE                                                         ELGCSIHS
02061          PERFORM DISPLAY-TRUE-ALCOHOL-DAYS.                       ELGCSIHS
02062                                                                   ELGCSIHS
02063 ************************************************************      ELGCSIHS
02064 *                                                          *      ELGCSIHS
02065 *        DISPLAY TRUE MENTAL DAYS                          *      ELGCSIHS
02066 *                                                          *      ELGCSIHS
02067 ************************************************************      ELGCSIHS
02068  DISPLAY-TRUE-MENTAL-DAYS.                                        ELGCSIHS
02069      MOVE PC-MENT                    TO WS-INFO.                  ELGCSIHS
02070      MOVE WS-MENTAL-DAYS             TO WS-DAYS-VALUE.            ELGCSIHS
02071      MOVE SPACES                     TO WS-INDICATOR.             ELGCSIHS
02072      PERFORM COMPRESS-AND-DISPLAY-OUTPUT-FO.                      ELGCSIHS
02073                                                                   ELGCSIHS
02074 ************************************************************      ELGCSIHS
02075 *                                                          *      ELGCSIHS
02076 *        DISPLAY TRUE DRUG DAYS                            *      ELGCSIHS
02077 *                                                          *      ELGCSIHS
02078 ************************************************************      ELGCSIHS
02079  DISPLAY-TRUE-DRUG-DAYS.                                          ELGCSIHS
02080      MOVE PC-DRUG                    TO WS-INFO.                  ELGCSIHS
02081      MOVE WS-DRUG-DAYS               TO WS-DAYS-VALUE.            ELGCSIHS
02082      MOVE SPACES                     TO WS-INDICATOR.             ELGCSIHS
02083      PERFORM COMPRESS-AND-DISPLAY-OUTPUT-FO.                      ELGCSIHS
02084                                                                   ELGCSIHS
02085 ************************************************************      ELGCSIHS
02086 *                                                          *      ELGCSIHS
02087 *        DISPLAY TRUE ALCOHOL DAYS                         *      ELGCSIHS
02088 *                                                          *      ELGCSIHS
02089 ************************************************************      ELGCSIHS
02090  DISPLAY-TRUE-ALCOHOL-DAYS.                                       ELGCSIHS
02091      MOVE PC-ALC                     TO WS-INFO.                  ELGCSIHS
02092      MOVE WS-ALCOHOL-DAYS            TO WS-DAYS-VALUE.            ELGCSIHS
02093      MOVE SPACES                     TO WS-INDICATOR.             ELGCSIHS
02094      PERFORM COMPRESS-AND-DISPLAY-OUTPUT-FO.                      ELGCSIHS
02095                                                                   ELGCSIHS
02096                                                                   ELGCSIHS
02097 ************************************************************      ELGCSIHS
02098 *                                                          *      ELGCSIHS
02099 *        SIGNAL MISSING CONTRACT                           *      ELGCSIHS
02100 *                                                          *      ELGCSIHS
02101 ************************************************************      ELGCSIHS
02102  SIGNAL-MISSING-CONTRACT.                                         ELGCSIHS
02103      SET CIA-ELSCONIS-DDN TO TRUE.                                ELGCSIHS
02104      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCSIHS
02105      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELGCSIHS
