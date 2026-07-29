00001  IDENTIFICATION DIVISION.                                         09/03/03
00002 *                                                                 ELGGXXC 
00003  PROGRAM-ID.         ELGGXXC.                                        LV002
00004 *                                                                 ELGGXXC 
00005  AUTHOR.             DAVID SECOR  OF  A.C.I.                      ELGGXXC 
00006 *                                                                 ELGGXXC 
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGGXXC 
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELGGXXC 
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGGXXC 
00010                      233 N. MICHIGAN AVE                          ELGGXXC 
00011                      CHICAGO, ILLINOIS 60601                      ELGGXXC 
00012 *                                                                 ELGGXXC 
00013  DATE-WRITTEN.       30-JUL-1987.                                 ELGGXXC 
00014 *                                                                 ELGGXXC 
00015  DATE-COMPILED.                                                   ELGGXXC 
00016 *                                                                 ELGGXXC 
00017  SECURITY.           COPYRIGHT 1987,                              ELGGXXC 
00018                      HEALTH CARE SERVICE CORPORATION              ELGGXXC 
00019 *                                                                 ELGGXXC 
00020 ******************************************************************ELGGXXC 
00021 *   ELGGXXC                                                       ELGGXXC 
00022 *                                                                 ELGGXXC 
00023 *                        PROGRAM ABSTRACT                         ELGGXXC 
00024 *                                                                 ELGGXXC 
00025 *   PROGRAM NAME:   E.L.S. GROUP SPECIFIC RELATED PROVIDERS       ELGGXXC 
00026 *                   COST CONTAINMENT SUBROUTINE                   ELGGXXC 
00027 *                                                                 ELGGXXC 
00028 *   PROGRAM I.D.:   ELGGXXC                                       ELGGXXC 
00029 *                                                                 ELGGXXC 
00030 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE FIELDS  ELGGXXC 
00031 *              FOR RELATED PROVIDERS COST CONTAINMENT.  SINCE     ELGGXXC 
00032 *              THE RECORD FORMATS FOR THE VARIOUS COST            ELGGXXC 
00033 *              CONTAINMENT RECORDS ARE IDENTICAL ONLY ONE         ELGGXXC 
00034 *              FORMAT HAS BEEN USED IN THIS PROGRAM.              ELGGXXC 
00035 *                                                                 ELGGXXC 
00036 *   RECORDS                                                       ELGGXXC 
00037 *   ACCESSED:  GMSC, GMSS, GPAC, GPAS, GPPO, GVLF, AND GVLP       ELGGXXC 
00038 *              ALL GROUP SPECIFIC TABULAR RECORDS                 ELGGXXC 
00039 *                                                                 ELGGXXC 
00040 ******************************************************************ELGGXXC 
00041 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       ELGGXXC 
00042 *       *-*         U P D A T E   H I S T O R Y         *-*       ELGGXXC 
00043 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*       ELGGXXC 
00044 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELGGXXC 
00045 *                                                                 ELGGXXC 
00046 *  ELS 2.0   07/28/87  DES  INCEPTION.                            ELGGXXC 
00047 *                                                                 ELGGXXC 
00048 *  ELS 2.0   08/19/87  DES  ADDED GPPO TABULAR AND CHANGED        ELGGXXC 
00049 *                           METHOD OF CALLING DBPIOC INTERFACE    ELGGXXC 
00050 *                                                                 ELGGXXC 
00051 *  ELS 2.0   10/05/87  AKK  CHANGED CODE TO SHOW FULL HOSPITAL    ELGGXXC 
00052 *                           NAME AND RECOMPILED PGM AFTER PPO     ELGGXXC 
00053 *                           INCLUDE/EXCLUDE COBOL NAME ADDED TO   ELGGXXC 
00054 *                           CODES MANUAL.                         ELGGXXC 
00055 *                                                                 ELGGXXC 
00056 *  ELS 2.0   01/14/88  REB  CHANGES MADE FOR NEW TOPIC 'ELTPPNET' ELGGXXC 
00057 *                           AND 'ELTPPO'. IT WILL LOOK AT HOW THE ELGGXXC 
00058 *                           CONTRACT PARTICIPATES IN PPO AND      ELGGXXC 
00059 *                           DISPLAY THE PROPER TEXT ACCORDINGLY.  ELGGXXC 
00060 *                                                                 ELGGXXC 
00061 *  ELS 2.0   01/15/88  REB  MADE CHANGES WITH TEXT DISPLAYED      ELGGXXC 
00062 *                                                                 ELGGXXC 
00063 *  ELS 2.0   01/22/88  REB  USERS ONLY WANT TRANSLATION FOR A     ELGGXXC 
00064 *                           'F' IN PPO INDICATOR.                 ELGGXXC 
00065 *                                                                 ELGGXXC 
00066 *  ELS 2.1   01/31/89  AKK  MADE CHANGES TO ACCOMODATE STORAGE    ELGGXXC 
00067 *                           MANAGEMENT CHANGES AND ALSO MADE      ELGGXXC 
00068 *                           A CHANGE TO THE 'DISPLAY PPO LOB      ELGGXXC 
00069 *                           AND STATUS' PARAGRAPH TO ACCOMODATE   ELGGXXC 
00070 *                           TWO COLUMN FORMAT CHANGES MADE TO     ELGGXXC 
00071 *                           ELTPPO.                               ELGGXXC 
00072 *                                                                 ELGGXXC 
00073 *  ELS 3.0   03/27/89  NAC  CONVERSION USING STRUCTURES VERS. 3.5 ELGGXXC 
00074 *                                                                 ELGGXXC 
00075 *  ELS 3.1   04/10/90  AKK  ADDED CODE VALUE I AND N TO NON-STAN  ELGGXXC 
00076 *                           DARD AND J,K,L TO STANDARD PPO LIST.  ELGGXXC 
00077 *                                                                 ELGGXXC 
00078 *  ELS 3.2   06/25/90  AKK  ADDED CODE TO SEARCH FOR PROVIDER     ELGGXXC 
00079 *                           NUMBER NOT FOUND IN PROVIDER FILE     ELGGXXC 
00080 *                           IN CODES MANUAL.                      ELGGXXC 
00081 *                           ALSO ADDED ADDITIONAL SETS IN CALL    ELGGXXC 
00082 *                           TO PROVIDER MASTER SO WE PICK UP ANY  ELGGXXC 
00083 *                           PROVIDERS WHETHER ACTIVE/INACTIVE ETC.ELGGXXC 
00084 *                                                                 ELGGXXC 
00085 *  ELS 3.3   11/14/90  JPB  CHANGED SIZE OF WS-GRP-SPC-PPO-IND TO ELGGXXC 
00086 *                           PIC X(2) TO ACCOMODATE SIMILAR CHANGE ELGGXXC 
00087 *                           TO GCG-PARTICIPAT-PROV-OPTION. ALSO   ELGGXXC 
00088 *                           CHANGED ITS 88-LEVEL ITEMS.           ELGGXXC 
00089 *                                                                 ELGGXXC 
00090 *  ELS 3.4   10/09/91  JPB  ADDED '0H' TO LIST OF NON-STANDARD-   ELGGXXC 
00091 *                           PPO'S AND ADDED LOGIC TO BRANCH TO    ELGGXXC 
00092 *                           ELKLOG IF THERE IS AN UNDEFINED PPO   ELGGXXC 
00093 *                           INDICATOR.                            ELGGXXC 
00094 *                                                                 ELGGXXC 
00095 *  ELS 3.5   01/06/93  AKK  ADDED SUPPORT FOR DISPLAY OF GRPO     ELGGXXC 
00096 *                           TABULAR.                              ELGGXXC 
00097 *                                                                 ELGGXXC 
00098 *  ELS 3.5   02/09/93  AKK  RPO AND PPO STANDARD/NON-STANDARD     ELGGXXC 
00099 *                           VALUES ARE DIFFERENT ADDED APPORPRIATEELGGXXC 
00100 *                           CODE.                                 ELGGXXC 
00101 *                                                                 ELGGXXC 
00102 *  ELS 3.6   04/15/94  AKK  ADDED PPO-IND '0Q', 0R', '0S',        ELGGXXC 
00103 *                           '0T' AS NON-STANDARD.                 ELGGXXC 
00104 *                                                                 ELGGXXC 
00105 *  ELS 3.6   06/09/94  AKK  ADDED PPO-IND '0U.                    ELGGXXC 
00106 *                                AS NON-STANDARD.                 ELGGXXC 
00107 *                                                                 ELGGXXC 
00108 *  ELS 3.7   ??/??/94  AKK  ADDED PPO-IND '0V.                    ELGGXXC 
00109 *                                AS NON-STANDARD.                 ELGGXXC 
00110 *                                                                 ELGGXXC 
00111 *  ELS 3.8   02/07/95  AKK  ADDED NEW RPO INDICATOR '03'          ELGGXXC 
00112 *                                AND PPO INDICATOR '10'           ELGGXXC 
00113 *                                BOTH NON-STANDARD                ELGGXXC 
00114 *                                                                 ELGGXXC 
00115 *  ELS 3.9   03/02/95  AKK  ADDED CODE TO PROCESS CPO.            ELGGXXC 
00116 *                                                                 ELGGXXC 
00117 *  ELS 3.10  02/26/96  AKK  ADDED CODE TO PROCESS PAN AND CBL     ELGGXXC 
00118 *                           MODELED AFTER CPO.                    ELGGXXC 
00119 *                           THERE HAVE BEEN ERRORS ON CPO.  CPO ISELGGXXC 
00120 *                           TO BE LIKE ALL OTHERS.. NON-STANDARD  ELGGXXC 
00121 *                           IS DETERMINED BY GROUP PART IND       ELGGXXC 
00122 *                                                                 ELGGXXC 
00123 *  ELS 3.11  07/09/97  AKK  ADDED NON-STANDARD VALUE 02 FOR       ELGGXXC 
00124 *                           CBL, PAN AND CPO.                     ELGGXXC 
00125 *                                                                 ELGGXXC 
00126 *  ELS 3.10  05/12/99  AKK  ADDED SUPPORT FOR BAE.                ELGGXXC 
00127 *                                                                 ELGGXXC 
00128 *  ELS 4.00  06/23/03  AKK  ADDED INTRO PHRASE FOR BAE FOUND      ELGGXXC 
00129 *                           WHILE REGRESSION TESTING FOR FILE EXP ELGGXXC 
00130 ******************************************************************ELGGXXC 
00131 /                                                                 ELGGXXC 
00132  ENVIRONMENT DIVISION.                                            ELGGXXC 
00133  CONFIGURATION SECTION.                                           ELGGXXC 
00134  SOURCE-COMPUTER.    IBM-3081.                                    ELGGXXC 
00135  OBJECT-COMPUTER.    IBM-3090.                                    ELGGXXC 
00136 /                                                                 ELGGXXC 
00137  DATA DIVISION.                                                   ELGGXXC 
00138  WORKING-STORAGE SECTION.                                         ELGGXXC 
00139  01  WS-BEGIN               PIC X(24)  VALUE                      ELGGXXC 
00140      'ELGGXXC WORKING STORAGE*'.                                  ELGGXXC 
00141 /     W O R K F I E L D S   A N D   S W I T C H E S               ELGGXXC 
00142  01  WS-MISC-WORK.                                                ELGGXXC 
00143      05  WS-SUB             PIC S9999  COMP SYNC VALUE ZEROES.    ELGGXXC 
00144      05  WS-CM-SUB          PIC S9999  COMP SYNC VALUE ZEROES.    ELGGXXC 
00145      05  WS-PVE             PIC X(4)   VALUE '#PVE'.              ELGGXXC 
00146      05  WS-I-E-IND         PIC X.                                ELGGXXC 
00147      05  WS-TAB-SLOT-NO     PIC 9(7).                             ELGGXXC 
00148      05  WS-FIRST-TIME-IND-VAL    PIC X.                          ELGGXXC 
00149        88  WS-FIRST-TIME-THIS-IND     VALUE 'Y'.                  ELGGXXC 
00150      05  WS-DID-GETMAIN-SWITCH  PIC X VALUE SPACE.                ELGGXXC 
00151        88  WS-DID-GETMAIN             VALUE 'Y'.                  ELGGXXC 
00152      05  WS-PROCESSING-SWITCH   PIC X VALUE SPACE.                ELGGXXC 
00153        88  PROCESSING-NORM-TABULAR    VALUE 'Y'.                  ELGGXXC 
00154      05  WS-PROCESSING-GBAE-SW  PIC X VALUE SPACE.                ELGGXXC 
00155        88  PROCESSING-GBAE            VALUE 'E'.                  ELGGXXC 
00156      05  WS-PROCESSING-GRPO-SW  PIC X VALUE SPACE.                ELGGXXC 
00157        88  PROCESSING-GRPO            VALUE 'R'.                  ELGGXXC 
00158      05  WS-PROCESSING-GCPO-SW  PIC X VALUE SPACE.                ELGGXXC 
00159        88  PROCESSING-GCPO            VALUE 'C'.                  ELGGXXC 
00160      05  WS-PROCESSING-GCBL-SW  PIC X VALUE SPACE.                ELGGXXC 
00161        88  PROCESSING-GCBL            VALUE 'B'.                  ELGGXXC 
00162      05  WS-PROCESSING-GPAN-SW  PIC X VALUE SPACE.                ELGGXXC 
00163        88  PROCESSING-GPAN            VALUE 'A'.                  ELGGXXC 
00164      05  WS-PROCESSING-GPPO-SW  PIC X VALUE SPACE.                ELGGXXC 
00165        88  PROCESSING-GPPO            VALUE 'P'.                  ELGGXXC 
00166                                                                   ELGGXXC 
00167      05  WS-GRP-SPC-PPO-IND PIC X(2)   VALUE SPACE.               ELGGXXC 
00168 ******** THIS WILL EXPLAIN WHAT EACH VALUE SIGNIFIES (PPO ONLY)   ELGGXXC 
00169 ******** '1-7'     = WILL INDICATE L-O-B AND STANDARD PPO         ELGGXXC 
00170 ******** 'G'       = THE BC/BS OPERATORMUST DECIDE PPO STATUS THE ELGGXXC 
00171 ******** REASON 'G' IS PUT IN STANDARD WE ONLY WANT TRANSLATION   ELGGXXC 
00172 ******** '8,9,A-E' = WILL INDICATE L-O-B AND NON-STANDARD PPO     ELGGXXC 
00173 ******** 'F'       = BC NON-STANDARD PPO, BS STANDARD PPO         ELGGXXC 
00174 ******** 'J,K,L    =OPER WILL DETERMINE PPO STATUS, ELS WILL HANDLELGGXXC 
00175 ********            AS STANDARD PPO                               ELGGXXC 
00176 ******** 'I'       =OPER WILL DETERMINE PPO STATUS, BUT GPPO TAB  ELGGXXC 
00177 ********            EXISTS AND WILL BE TREATED BY ELS AS NON-STANDELGGXXC 
00178 ******** 'N'       =BLUE CROSS STANDARD, BLUSE SHIELD MUTUAL PART ELGGXXC 
00179 ********            PROGRAM MEMBER- ELS WILL TREAT AS NON STANDARDELGGXXC 
00180 ********            AS PPO TAB WILL BE CODED PER LH 04/10/90      ELGGXXC 
00181 ******** 'M'       =BLUE CROSS NON-STANDARD LIST/BLUE SHEILD STAN-ELGGXXC 
00182 ********            DARD LIST.                                    ELGGXXC 
00183          88  STANDARD-PPO              VALUE                      ELGGXXC 
00184              '01' '02' '03' '04' '05' '06' '07' '0F' '0G' '0J'    ELGGXXC 
00185              '0K' '0L'.                                           ELGGXXC 
00186          88  NON-STANDARD-PPO          VALUE                      ELGGXXC 
00187              '08' '09' '10' '0B' '0C' '0D' '0E' '0H' '0I'         ELGGXXC 
00188              '0M' '0N' '0P' '0Q' '0R' '0S' '0T' '0U' '0V'.        ELGGXXC 
00189                                                                   ELGGXXC 
00190      05  WS-GRP-SPC-BAE-IND PIC X(2)   VALUE SPACE.               ELGGXXC 
00191          88  STANDARD-BAE              VALUE                      ELGGXXC 
00192              '01'.                                                ELGGXXC 
00193          88  NON-STANDARD-BAE          VALUE                      ELGGXXC 
00194              '02' '03'.                                           ELGGXXC 
00195                                                                   ELGGXXC 
00196      05  WS-GRP-SPC-RPO-IND PIC X(2)   VALUE SPACE.               ELGGXXC 
00197          88  STANDARD-RPO              VALUE                      ELGGXXC 
00198              '01'.                                                ELGGXXC 
00199          88  NON-STANDARD-RPO          VALUE                      ELGGXXC 
00200              '02' '03'.                                           ELGGXXC 
00201                                                                   ELGGXXC 
00202      05  WS-GRP-SPC-CPO-IND PIC X(2)   VALUE SPACE.               ELGGXXC 
00203          88  STANDARD-CPO              VALUE                      ELGGXXC 
00204              '01'.                                                ELGGXXC 
00205          88  NON-STANDARD-CPO          VALUE                      ELGGXXC 
00206              '02'.                                                ELGGXXC 
00207                                                                   ELGGXXC 
00208      05  WS-GRP-SPC-CBL-IND PIC X(2)   VALUE SPACE.               ELGGXXC 
00209          88  STANDARD-CBL              VALUE                      ELGGXXC 
00210              '01'.                                                ELGGXXC 
00211          88  NON-STANDARD-CBL          VALUE                      ELGGXXC 
00212              '02'.                                                ELGGXXC 
00213                                                                   ELGGXXC 
00214      05  WS-GRP-SPC-PAN-IND PIC X(2)   VALUE SPACE.               ELGGXXC 
00215          88  STANDARD-PAN              VALUE                      ELGGXXC 
00216              '01'.                                                ELGGXXC 
00217          88  NON-STANDARD-PAN          VALUE                      ELGGXXC 
00218              '02'.                                                ELGGXXC 
00219                                                                   ELGGXXC 
00220      05  WS-TAB-TYPE-DEF.                                         ELGGXXC 
00221        10  WS-GMSC          PIC X(6)   VALUE '#GMSC '.            ELGGXXC 
00222        10  WS-GMSS          PIC X(6)   VALUE '#GMSS '.            ELGGXXC 
00223        10  WS-GPAC          PIC X(6)   VALUE '#GPAC '.            ELGGXXC 
00224        10  WS-GPAS          PIC X(6)   VALUE '#GPAS '.            ELGGXXC 
00225        10  WS-GPPO          PIC X(6)   VALUE '#GPPO '.            ELGGXXC 
00226        10  WS-GBAE          PIC X(6)   VALUE '#GBAE '.            ELGGXXC 
00227        10  WS-GRPO          PIC X(6)   VALUE '#GRPO '.            ELGGXXC 
00228        10  WS-GCPO          PIC X(6)   VALUE '#GCPO '.            ELGGXXC 
00229        10  WS-GCBL          PIC X(6)   VALUE '#GCBL '.            ELGGXXC 
00230        10  WS-GPAN          PIC X(6)   VALUE '#GPAN '.            ELGGXXC 
00231        10  WS-GVLF          PIC X(6)   VALUE '#GVLF '.            ELGGXXC 
00232        10  WS-GVLP          PIC X(6)   VALUE '#GVLP '.            ELGGXXC 
00233      05  WS-TAB-ID-TABLE   REDEFINES   WS-TAB-TYPE-DEF.           ELGGXXC 
00234        10  WS-TAB-TYPE      PIC X(6) OCCURS  12 TIMES             ELGGXXC 
00235                             INDEXED BY  WS-IDX.                   ELGGXXC 
00236                                                                   ELGGXXC 
00237  01  WS-MESSAGE-AREA.                                             ELGGXXC 
00238      05  WS-PPO-APPLIES         PIC X(41)  VALUE                  ELGGXXC 
00239          'THE PREFERRED PROVIDER OPTION APPLIES TO '.             ELGGXXC 
00240      05  WS-RPO-APPLIES         PIC X(42)  VALUE                  ELGGXXC 
00241          'THE RESTRICTED PROVIDER OPTION APPLIES TO '.            ELGGXXC 
00242      05  WS-BAE-APPLIES         PIC X(44)  VALUE                  ELGGXXC 
00243          'THE BLUE ADVANTAGE ENTREPRENEUR APPLIES TO '.           ELGGXXC 
00244      05  WS-CPO-APPLIES         PIC X(70)  VALUE                  ELGGXXC 
00245          'THE COMMUNITY PARTICIPATING OPTION APPLIES TO THE FOLLOWELGGXXC 
00246 -        'ING PROVIDERS:'.                                        ELGGXXC 
00247      05  WS-CBL-APPLIES         PIC X(61)  VALUE                  ELGGXXC 
00248          'THE COMMUNITY BLUE OPTION APPLIES TO THE FOLLOWING PROVIELGGXXC 
00249 -        'DERS:'.                                                 ELGGXXC 
00250      05  WS-PAN-APPLIES         PIC X(74)  VALUE                  ELGGXXC 
00251          'THE PREFERRED ANCILLARY NETWORK OPTION APPLIES TO THE FOELGGXXC 
00252 -        'LLOWING PROVIDERS:'.                                    ELGGXXC 
00253      05  WS-CORP-LIST           PIC X(79)  VALUE 'THIS PPO CONTRACELGGXXC 
00254 -        'T USES THE CORPORATE PPO LIST AND IN ADDITION '.        ELGGXXC 
00255      05  WS-CBL-PHRASE         PIC X(79)  VALUE  'THIS CBL CONTRACELGGXXC 
00256 -        'T '.                                                    ELGGXXC 
00257      05  WS-PAN-PHRASE         PIC X(79)  VALUE  'THIS PAN CONTRACELGGXXC 
00258 -        'T '.                                                    ELGGXXC 
00259      05  WS-RPO-PHRASE         PIC X(79)  VALUE  'THIS RPO CONTRACELGGXXC 
00260 -        'T '.                                                    ELGGXXC 
00261      05  WS-BAE-PHRASE         PIC X(79)  VALUE  'THIS BAE CONTRACELGGXXC 
00262 -        'T '.                                                    ELGGXXC 
00263      05  WS-CPO-PHRASE         PIC X(79)  VALUE  'THIS CPO CONTRACELGGXXC 
00264 -        'T '.                                                    ELGGXXC 
00265      05  WS-DIFFER-CORP-LIST-A  PIC X(69)  VALUE 'THIS PPO CONTRACELGGXXC 
00266 -        'T HAS THE FOLLOWING PREFERRED PROVIDERS (IN PLACE OF '. ELGGXXC 
00267      05  WS-DIFFER-CORP-LIST-B  PIC X(26)  VALUE                  ELGGXXC 
00268          'THE CORPORATE PPO LIST) : '.                            ELGGXXC 
00269      05  WS-DIFFER-LIST-A  PIC X(75)  VALUE 'THIS RPO CONTRACT HASELGGXXC 
00270 -        ' THE FOLLOWING RESTRICTED PROVIDERS (IN PLACE OF THE '. ELGGXXC 
00271      05  WS-DIFFER-LIST-B  PIC X(21)  VALUE                       ELGGXXC 
00272          'STANDARD RPO LIST) : '.                                 ELGGXXC 
00273      05  WS-DIFFER-LIST-E  PIC X(75)  VALUE 'THIS BAE CONTRACT HASELGGXXC 
00274 -        ' THE FOLLOWING ?????????? PROVIDERS (IN PLACE OF THE '. ELGGXXC 
00275      05  WS-DIFFER-LIST-E1 PIC X(21)  VALUE                       ELGGXXC 
00276          'STANDARD BAE LIST) : '.                                 ELGGXXC 
00277      05  WS-SEE-CONTRACT-SENT   PIC X(142) VALUE 'IF THE PPO PROGRELGGXXC 
00278 -        'AM FOR THIS GROUP/SECTION USES A NON-STANDARD NETWORK OFELGGXXC 
00279 -        ' PPO PROVIDERS, SEE CONTRACT DOCUMENTATION FOR THE LIST ELGGXXC 
00280 -        'OF EXCEPTIONS.'.                                        ELGGXXC 
00281      05  WS-SEE-BAE-CONTRACT    PIC X(142) VALUE 'IF THE BAE PROGRELGGXXC 
00282 -        'AM FOR THIS GROUP/SECTION USES A NON-STANDARD NETWORK OFELGGXXC 
00283 -        ' BAE PROVIDERS, SEE CONTRACT DOCUMENTATION FOR THE LIST ELGGXXC 
00284 -        'OF EXCEPTIONS.'.                                        ELGGXXC 
00285      05  WS-SEE-RPO-CONTRACT    PIC X(142) VALUE 'IF THE RPO PROGRELGGXXC 
00286 -        'AM FOR THIS GROUP/SECTION USES A NON-STANDARD NETWORK OFELGGXXC 
00287 -        ' RPO PROVIDERS, SEE CONTRACT DOCUMENTATION FOR THE LIST ELGGXXC 
00288 -        'OF EXCEPTIONS.'.                                        ELGGXXC 
00289      05  WS-SEE-CPO-CONTRACT    PIC X(142) VALUE 'IF THE CPO PROGRELGGXXC 
00290 -        'AM FOR THIS GROUP/SECTION USES A NON-STANDARD NETWORK OFELGGXXC 
00291 -        ' CPO PROVIDERS, SEE CONTRACT DOCUMENTATION FOR THE LIST ELGGXXC 
00292 -        'OF EXCEPTIONS.'.                                        ELGGXXC 
00293  01  WS-END                     PIC X(24)  VALUE                  ELGGXXC 
00294      '*** ELGGXXC W/S ENDS ***'.                                  ELGGXXC 
00295 /             L I N K A G E   S E C T I O N                       ELGGXXC 
00296  LINKAGE SECTION.                                                 ELGGXXC 
00297  01  DFHCOMMAREA.                                                 ELGGXXC 
00298      COPY ELSCOMMC.                                               ELGGXXC 
00299 /    C O M M O N   I N T E R F A C E   A R E A                    ELGGXXC 
00300      COPY ELSCIA2C.                                               ELGGXXC 
00301 /    C O D E S   M A N U A L   D E S C R I P T I O N   L I N E S  ELGGXXC 
00302      COPY ELSCMDSC.                                               ELGGXXC 
00303 /    C O D E S   M A N U A L   C N T L .   B L O C K              ELGGXXC 
00304      COPY ELSELOGC.                                               ELGGXXC 
00305 /    E R R O R   L O G   R E C O R D   D E S C R I P T I O N      ELGGXXC 
00306      COPY ELSCMIFC.                                               ELGGXXC 
00307 /    I / O   P A R A M E T E R   B L O C K                        ELGGXXC 
00308      COPY ELSIOPMC.                                               ELGGXXC 
00309 /    W O R K   A R E A   T O   B U I L D   K E Y S                ELGGXXC 
00310      COPY ELSKEYSC.                                               ELGGXXC 
00311 /    C N T L   B L O C K   -   O U T P U T   P A G E   B L D R    ELGGXXC 
00312      COPY ELSOUTPC.                                               ELGGXXC 
00313 /    T E X T   C O M P R E S S I O N   W O R K   A R E A          ELGGXXC 
00314      COPY ELSTCWAC.                                               ELGGXXC 
00315 /    S E L E C T O R   S T A T U S   C O N T R O L   B L O C K    ELGGXXC 
00316      COPY ELSSSCBC.                                               ELGGXXC 
00317 /    S U B R O U T I N E   P A R A M E T E R   L I S T            ELGGXXC 
00318      COPY ELSSRTPC.                                               ELGGXXC 
00319 /    G R O U P   S P E C I F I C   # G M S C   T A B U L A R      ELGGXXC 
00320  01  GSL-TABULAR-REC-AREA.                                        ELGGXXC 
00321      COPY GCTGMSCC.                                               ELGGXXC 
00322 /    G R O U P   S P E C I F I C   R E C O R D ( O N L Y)         ELGGXXC 
00323  01  GROUP-SPECIFIC-RECORD.                                       ELGGXXC 
00324      COPY GCGROUPC.                                               ELGGXXC 
00325 /    G R O U P   S P E C I F I C   # G P P O   T A B U L A R      ELGGXXC 
00326  01  GSW-TABULAR-REC-AREA.                                        ELGGXXC 
00327      COPY GCTGPPOC.                                               ELGGXXC 
00328 /    P R O V I D E R   N O .   T O   N A M E   D B   P A R M S    ELGGXXC 
00329  01  PDB-IO-AREA.                                                 ELGGXXC 
00330      COPY DBPIOPMC.                                               ELGGXXC 
00331 /    P R O V I D E R   M A S T E R   R E C O R D   D E S C R .    ELGGXXC 
00332  01  PROVIDER-MSTR-REC.                                           ELGGXXC 
00333      COPY PROVMSTR.                                               ELGGXXC 
00334  PROCEDURE DIVISION.                                              ELGGXXC 
00335 ************************************************************      ELGGXXC 
00336 *                                                          *      ELGGXXC 
00337 *        GXXC TABULAR MAINLINE                             *      ELGGXXC 
00338 *                                                          *      ELGGXXC 
00339 ************************************************************      ELGGXXC 
00340  GXXC-TABULAR-MAINLINE.                                           ELGGXXC 
00341      PERFORM INITIALIZATION-ROUTINE.                              ELGGXXC 
00342      PERFORM MAIN-ROUTINE.                                        ELGGXXC 
00343      GOBACK.                                                      ELGGXXC 
00344                                                                   ELGGXXC 
00345 ************************************************************      ELGGXXC 
00346 *                                                          *      ELGGXXC 
00347 *        INITIALIZATION ROUTINE                            *      ELGGXXC 
00348 *                                                          *      ELGGXXC 
00349 ************************************************************      ELGGXXC 
00350  INITIALIZATION-ROUTINE.                                          ELGGXXC 
00351      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELGGXXC 
00352          PERFORM COMMAREA-LENGTH-ERROR.                           ELGGXXC 
00353      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGGXXC 
00354          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELGGXXC 
00355      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGGXXC 
00356      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00357          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELGGXXC 
00358      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGGXXC 
00359      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00360          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELGGXXC 
00361      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGGXXC 
00362      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00363          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELGGXXC 
00364      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGGXXC 
00365      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00366          ADDRESS OF SRP-SUBROUTINE-PARAMETERS.                    ELGGXXC 
00367      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGGXXC 
00368      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00369          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELGGXXC 
00370      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGGXXC 
00371      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00372          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELGGXXC 
00373      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGGXXC 
00374      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00375          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELGGXXC 
00376      INITIALIZE CMF-CODES-MANUAL-INTERFACE                        ELGGXXC 
00377                 TCAR-FROM-AREA.                                   ELGGXXC 
00378      SET CIA-DBPIOPM-DDN  TO TRUE.                                ELGGXXC 
00379      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00380          ADDRESS OF PDB-IO-AREA.                                  ELGGXXC 
00381      IF CIA-RC-PTR-NULL                                           ELGGXXC 
00382          PERFORM GETMAIN-IO-PARM-AREA.                            ELGGXXC 
00383      IF WS-DID-GETMAIN                                            ELGGXXC 
00384          PERFORM DO-ADDRESS-OF-PDB.                               ELGGXXC 
00385      MOVE 'Y'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXC 
00386                                                                   ELGGXXC 
00387 ************************************************************      ELGGXXC 
00388 *                                                          *      ELGGXXC 
00389 *        DO ADDRESS OF PDB                                 *      ELGGXXC 
00390 *                                                          *      ELGGXXC 
00391 ************************************************************      ELGGXXC 
00392  DO-ADDRESS-OF-PDB.                                               ELGGXXC 
00393      SET CIA-DBPIOPM-DDN  TO TRUE.                                ELGGXXC 
00394      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00395          ADDRESS OF PDB-IO-AREA.                                  ELGGXXC 
00396                                                                   ELGGXXC 
00397 ************************************************************      ELGGXXC 
00398 *                                                          *      ELGGXXC 
00399 *        COMMAREA LENGTH ERROR                             *      ELGGXXC 
00400 *                                                          *      ELGGXXC 
00401 ************************************************************      ELGGXXC 
00402  COMMAREA-LENGTH-ERROR.                                           ELGGXXC 
00403      SET CIA-AB-DFHCOMMAREA  TO  TRUE.                            ELGGXXC 
00404      EXEC CICS  ABEND  ABCODE(CIA-ABCODE) END-EXEC.               ELGGXXC 
00405                                                                   ELGGXXC 
00406 ************************************************************      ELGGXXC 
00407 *                                                          *      ELGGXXC 
00408 *        MAIN ROUTINE                                      *      ELGGXXC 
00409 *                                                          *      ELGGXXC 
00410 ************************************************************      ELGGXXC 
00411  MAIN-ROUTINE.                                                    ELGGXXC 
00412      PERFORM SEARCH-COST-CONTAINMENT-TABLE.                       ELGGXXC 
00413      EVALUATE TRUE                                                ELGGXXC 
00414        WHEN SRP-TABULAR-ID  =  WS-GPPO                            ELGGXXC 
00415            SET PROCESSING-GPPO TO TRUE                            ELGGXXC 
00416            PERFORM PROCESS-GPPO-REQUEST                           ELGGXXC 
00417        WHEN SRP-TABULAR-ID  =  WS-GBAE                            ELGGXXC 
00418            SET PROCESSING-GBAE TO TRUE                            ELGGXXC 
00419            PERFORM PROCESS-GPPO-REQUEST                           ELGGXXC 
00420        WHEN SRP-TABULAR-ID  =  WS-GRPO                            ELGGXXC 
00421            SET PROCESSING-GRPO TO TRUE                            ELGGXXC 
00422            PERFORM PROCESS-GPPO-REQUEST                           ELGGXXC 
00423        WHEN SRP-TABULAR-ID  =  WS-GCPO                            ELGGXXC 
00424            SET PROCESSING-GCPO TO TRUE                            ELGGXXC 
00425            PERFORM PROCESS-GPPO-REQUEST                           ELGGXXC 
00426        WHEN SRP-TABULAR-ID  =  WS-GCBL                            ELGGXXC 
00427            SET PROCESSING-GCBL TO TRUE                            ELGGXXC 
00428            PERFORM PROCESS-GPPO-REQUEST                           ELGGXXC 
00429        WHEN SRP-TABULAR-ID  =  WS-GPAN                            ELGGXXC 
00430            SET PROCESSING-GPAN TO TRUE                            ELGGXXC 
00431            PERFORM PROCESS-GPPO-REQUEST                           ELGGXXC 
00432        WHEN OTHER                                                 ELGGXXC 
00433            PERFORM PROCESS-NORM-PROV-ACCESS                       ELGGXXC 
00434      END-EVALUATE.                                                ELGGXXC 
00435                                                                   ELGGXXC 
00436 ************************************************************      ELGGXXC 
00437 *                                                          *      ELGGXXC 
00438 *        PROCESS GPPO REQUEST                              *      ELGGXXC 
00439 *                                                          *      ELGGXXC 
00440 ************************************************************      ELGGXXC 
00441  PROCESS-GPPO-REQUEST.                                            ELGGXXC 
00442      PERFORM DISPLAY-PPO-LOB-AND-STATUS.                          ELGGXXC 
00443      EVALUATE TRUE                                                ELGGXXC 
00444      WHEN PROCESSING-GBAE                                         ELGGXXC 
00445         PERFORM EVALUATE-BAE                                      ELGGXXC 
00446      WHEN PROCESSING-GRPO                                         ELGGXXC 
00447         PERFORM EVALUATE-RPO                                      ELGGXXC 
00448      WHEN PROCESSING-GPPO                                         ELGGXXC 
00449         PERFORM EVALUATE-PPO                                      ELGGXXC 
00450      WHEN PROCESSING-GCPO                                         ELGGXXC 
00451         PERFORM EVALUATE-CPO                                      ELGGXXC 
00452      WHEN PROCESSING-GCBL                                         ELGGXXC 
00453         PERFORM EVALUATE-CBL                                      ELGGXXC 
00454      WHEN PROCESSING-GPAN                                         ELGGXXC 
00455         PERFORM EVALUATE-PAN                                      ELGGXXC 
00456      END-EVALUATE.                                                ELGGXXC 
00457                                                                   ELGGXXC 
00458 ***********************************************************       ELGGXXC 
00459 *                                                          *      ELGGXXC 
00460 *        EVALUATE BAE                                      *      ELGGXXC 
00461 *                                                          *      ELGGXXC 
00462 ************************************************************      ELGGXXC 
00463  EVALUATE-BAE.                                                    ELGGXXC 
00464      EVALUATE TRUE                                                ELGGXXC 
00465         WHEN STANDARD-BAE                                         ELGGXXC 
00466              CONTINUE                                             ELGGXXC 
00467         WHEN NON-STANDARD-BAE                                     ELGGXXC 
00468              PERFORM PROCESS-GPPO-PROV-ACCESS                     ELGGXXC 
00469         WHEN OTHER                                                ELGGXXC 
00470              PERFORM CONSTRUCT-SEE-CONTRACT-SENT                  ELGGXXC 
00471      END-EVALUATE.                                                ELGGXXC 
00472                                                                   ELGGXXC 
00473 ***********************************************************       ELGGXXC 
00474 *                                                          *      ELGGXXC 
00475 *        EVALUATE RPO                                      *      ELGGXXC 
00476 *                                                          *      ELGGXXC 
00477 ************************************************************      ELGGXXC 
00478  EVALUATE-RPO.                                                    ELGGXXC 
00479      EVALUATE TRUE                                                ELGGXXC 
00480         WHEN STANDARD-RPO                                         ELGGXXC 
00481              CONTINUE                                             ELGGXXC 
00482         WHEN NON-STANDARD-RPO                                     ELGGXXC 
00483              PERFORM PROCESS-GPPO-PROV-ACCESS                     ELGGXXC 
00484         WHEN OTHER                                                ELGGXXC 
00485              PERFORM CONSTRUCT-SEE-CONTRACT-SENT                  ELGGXXC 
00486      END-EVALUATE.                                                ELGGXXC 
00487                                                                   ELGGXXC 
00488 ************************************************************      ELGGXXC 
00489 *                                                          *      ELGGXXC 
00490 *        EVALUATE PPO                                      *      ELGGXXC 
00491 *                                                          *      ELGGXXC 
00492 ************************************************************      ELGGXXC 
00493  EVALUATE-PPO.                                                    ELGGXXC 
00494      EVALUATE TRUE                                                ELGGXXC 
00495         WHEN STANDARD-PPO                                         ELGGXXC 
00496              CONTINUE                                             ELGGXXC 
00497         WHEN NON-STANDARD-PPO                                     ELGGXXC 
00498              PERFORM PROCESS-GPPO-PROV-ACCESS                     ELGGXXC 
00499         WHEN OTHER                                                ELGGXXC 
00500              PERFORM CONSTRUCT-SEE-CONTRACT-SENT                  ELGGXXC 
00501      END-EVALUATE.                                                ELGGXXC 
00502                                                                   ELGGXXC 
00503 ************************************************************      ELGGXXC 
00504 *                                                          *      ELGGXXC 
00505 *        EVALUATE CPO                                      *      ELGGXXC 
00506 *                                                          *      ELGGXXC 
00507 ************************************************************      ELGGXXC 
00508  EVALUATE-CPO.                                                    ELGGXXC 
00509      EVALUATE TRUE                                                ELGGXXC 
00510         WHEN STANDARD-CPO                                         ELGGXXC 
00511              CONTINUE                                             ELGGXXC 
00512         WHEN NON-STANDARD-CPO                                     ELGGXXC 
00513              PERFORM PROCESS-GPPO-PROV-ACCESS                     ELGGXXC 
00514         WHEN OTHER                                                ELGGXXC 
00515              PERFORM CONSTRUCT-SEE-CONTRACT-SENT                  ELGGXXC 
00516      END-EVALUATE.                                                ELGGXXC 
00517                                                                   ELGGXXC 
00518 ************************************************************      ELGGXXC 
00519 *                                                          *      ELGGXXC 
00520 *        EVALUATE CBL                                      *      ELGGXXC 
00521 *                                                          *      ELGGXXC 
00522 ************************************************************      ELGGXXC 
00523  EVALUATE-CBL.                                                    ELGGXXC 
00524      EVALUATE TRUE                                                ELGGXXC 
00525         WHEN STANDARD-CBL                                         ELGGXXC 
00526              CONTINUE                                             ELGGXXC 
00527         WHEN NON-STANDARD-CBL                                     ELGGXXC 
00528              PERFORM PROCESS-GPPO-PROV-ACCESS                     ELGGXXC 
00529         WHEN OTHER                                                ELGGXXC 
00530              PERFORM CONSTRUCT-SEE-CONTRACT-SENT                  ELGGXXC 
00531      END-EVALUATE.                                                ELGGXXC 
00532                                                                   ELGGXXC 
00533 ************************************************************      ELGGXXC 
00534 *                                                          *      ELGGXXC 
00535 *        EVALUATE PAN                                      *      ELGGXXC 
00536 *                                                          *      ELGGXXC 
00537 ************************************************************      ELGGXXC 
00538  EVALUATE-PAN.                                                    ELGGXXC 
00539      EVALUATE TRUE                                                ELGGXXC 
00540         WHEN STANDARD-PAN                                         ELGGXXC 
00541              CONTINUE                                             ELGGXXC 
00542         WHEN NON-STANDARD-PAN                                     ELGGXXC 
00543              PERFORM PROCESS-GPPO-PROV-ACCESS                     ELGGXXC 
00544         WHEN OTHER                                                ELGGXXC 
00545              PERFORM CONSTRUCT-SEE-CONTRACT-SENT                  ELGGXXC 
00546      END-EVALUATE.                                                ELGGXXC 
00547                                                                   ELGGXXC 
00548 ************************************************************      ELGGXXC 
00549 *                                                          *      ELGGXXC 
00550 *        CONSTRUCT-SEE-CONTRACT-SENT                       *      ELGGXXC 
00551 *                                                          *      ELGGXXC 
00552 ************************************************************      ELGGXXC 
00553  CONSTRUCT-SEE-CONTRACT-SENT.                                     ELGGXXC 
00554      ADD +1 TO TCAR-FROM-SUB.                                     ELGGXXC 
00555      MOVE WS-SEE-CONTRACT-SENT TO TCAR-FROM-LINE (TCAR-FROM-SUB). ELGGXXC 
00556      PERFORM WRITE-TO-LOGFILE.                                    ELGGXXC 
00557                                                                   ELGGXXC 
00558 ************************************************************      ELGGXXC 
00559 *                                                          *      ELGGXXC 
00560 *        WRITE TO LOGFILE                                  *      ELGGXXC 
00561 *                                                          *      ELGGXXC 
00562 ************************************************************      ELGGXXC 
00563  WRITE-TO-LOGFILE.                                                ELGGXXC 
00564      PERFORM ESTABLISH-ADDRESS-OF-LOG-AREA.                       ELGGXXC 
00565      INITIALIZE LG-LOG-RECORD.                                    ELGGXXC 
00566      EVALUATE TRUE                                                ELGGXXC 
00567      WHEN PROCESSING-GRPO                                         ELGGXXC 
00568         MOVE GCG-RPO-INDICATOR TO LG-CODE-VALUE                   ELGGXXC 
00569         MOVE 'RPO-INDICATOR'     TO LG-ELEMENT-NAME               ELGGXXC 
00570      WHEN PROCESSING-GPPO                                         ELGGXXC 
00571         MOVE GCG-PARTICIPAT-PROV-OPTION TO LG-CODE-VALUE          ELGGXXC 
00572         MOVE 'PARTICIP-PROV-OPTION'     TO LG-ELEMENT-NAME        ELGGXXC 
00573      WHEN PROCESSING-GCPO                                         ELGGXXC 
00574         MOVE GCG-CPO-PARTICIPATION-IND TO LG-CODE-VALUE           ELGGXXC 
00575         MOVE 'CPO-PARTICIPATION-IND'     TO LG-ELEMENT-NAME       ELGGXXC 
00576      WHEN PROCESSING-GCBL                                         ELGGXXC 
00577         MOVE GCG-CBL-PARTICIPATION-IND TO LG-CODE-VALUE           ELGGXXC 
00578         MOVE 'CBL-PARTICIPATION-IND'     TO LG-ELEMENT-NAME       ELGGXXC 
00579      WHEN PROCESSING-GPAN                                         ELGGXXC 
00580         MOVE GCG-PAN-PARTICIPATION-IND TO LG-CODE-VALUE           ELGGXXC 
00581         MOVE 'PAN-PARTICIPATION-IND'     TO LG-ELEMENT-NAME       ELGGXXC 
00582      END-EVALUATE.                                                ELGGXXC 
00583      MOVE 'GROUP'                    TO LG-RECORD-PREFIX.         ELGGXXC 
00584      MOVE LENGTH OF LG-LOG-RECORD    TO LG-LOG-LENGTH.            ELGGXXC 
00585      MOVE 0                          TO LG-DE-NUMBER.             ELGGXXC 
00586      SET LG-C-V-LOGIC                TO TRUE.                     ELGGXXC 
00587      CALL 'ELKLOG' USING DFHEIBLK                                 ELGGXXC 
00588                          DFHCOMMAREA.                             ELGGXXC 
00589                                                                   ELGGXXC 
00590 ************************************************************      ELGGXXC 
00591 *                                                          *      ELGGXXC 
00592 *     ESTABLISH ADDRESS OF LOG AREA                        *      ELGGXXC 
00593 *                                                          *      ELGGXXC 
00594 ************************************************************      ELGGXXC 
00595  ESTABLISH-ADDRESS-OF-LOG-AREA.                                   ELGGXXC 
00596      SET CIA-ELSELOG-DDN TO TRUE.                                 ELGGXXC 
00597      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00598         ADDRESS OF LG-LOG-RECORD.                                 ELGGXXC 
00599      IF CIA-RC-PTR-NULL                                           ELGGXXC 
00600         PERFORM ALLOCATE-LOG-REC-AREA                             ELGGXXC 
00601      ELSE                                                         ELGGXXC 
00602         IF NOT CIA-RC-OK                                          ELGGXXC 
00603            PERFORM SIGNAL-LOGIC-ERROR.                            ELGGXXC 
00604                                                                   ELGGXXC 
00605 ************************************************************      ELGGXXC 
00606 *                                                          *      ELGGXXC 
00607 *    SIGNAL LOGIC ERROR                                    *      ELGGXXC 
00608 *                                                          *      ELGGXXC 
00609 ************************************************************      ELGGXXC 
00610  SIGNAL-LOGIC-ERROR.                                              ELGGXXC 
00611      SET CIA-AB-UNDEF TO TRUE.                                    ELGGXXC 
00612      EXEC CICS ABEND                                              ELGGXXC 
00613                ABCODE(CIA-ABCODE)                                 ELGGXXC 
00614                END-EXEC.                                          ELGGXXC 
00615                                                                   ELGGXXC 
00616 ************************************************************      ELGGXXC 
00617 *                                                          *      ELGGXXC 
00618 *    ALLOCATE LOG REC AREA                                 *      ELGGXXC 
00619 *                                                          *      ELGGXXC 
00620 ************************************************************      ELGGXXC 
00621  ALLOCATE-LOG-REC-AREA.                                           ELGGXXC 
00622      COMPUTE CIA-AREA-LEN = LENGTH OF LG-LOG-RECORD.              ELGGXXC 
00623      SET CIA-STG-GETMAIN TO TRUE.                                 ELGGXXC 
00624      CALL 'ELUSTGMG' USING DFHEIBLK                               ELGGXXC 
00625                            DFHCOMMAREA.                           ELGGXXC 
00626      SET CIA-ELSELOG-DDN TO TRUE.                                 ELGGXXC 
00627      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00628                     ADDRESS OF LG-LOG-RECORD.                     ELGGXXC 
00629                                                                   ELGGXXC 
00630 ************************************************************      ELGGXXC 
00631 *                                                          *      ELGGXXC 
00632 *        DISPLAY PPO LOB AND STATUS                        *      ELGGXXC 
00633 * THIS ALSO DISPLAYS RPO INDICATOR                         *      ELGGXXC 
00634 ************************************************************      ELGGXXC 
00635  DISPLAY-PPO-LOB-AND-STATUS.                                      ELGGXXC 
00636 *    IF SRP-TABULAR-SLOT-NO NOT = ZERO                            ELGGXXC 
00637          PERFORM INSERT-BLANK-LINE.                               ELGGXXC 
00638      EVALUATE TRUE                                                ELGGXXC 
00639         WHEN PROCESSING-GBAE                                      ELGGXXC 
00640            MOVE GCG-BAE-INDICATOR TO WS-GRP-SPC-BAE-IND           ELGGXXC 
00641         WHEN PROCESSING-GRPO                                      ELGGXXC 
00642            MOVE GCG-RPO-INDICATOR TO WS-GRP-SPC-RPO-IND           ELGGXXC 
00643         WHEN PROCESSING-GPPO                                      ELGGXXC 
00644            MOVE GCG-PARTICIPAT-PROV-OPTION TO WS-GRP-SPC-PPO-IND  ELGGXXC 
00645         WHEN PROCESSING-GCBL                                      ELGGXXC 
00646            MOVE GCG-CBL-PARTICIPATION-IND TO WS-GRP-SPC-CBL-IND   ELGGXXC 
00647         WHEN PROCESSING-GPAN                                      ELGGXXC 
00648            MOVE GCG-PAN-PARTICIPATION-IND TO WS-GRP-SPC-PAN-IND   ELGGXXC 
00649         WHEN PROCESSING-GCPO                                      ELGGXXC 
00650            MOVE GCG-CPO-PARTICIPATION-IND TO WS-GRP-SPC-CPO-IND   ELGGXXC 
00651      END-EVALUATE.                                                ELGGXXC 
00652      PERFORM TRANSLATE-PPO-INDICATOR.                             ELGGXXC 
00653      MOVE +1             TO TCAR-FROM-SUB.                        ELGGXXC 
00654      EVALUATE TRUE                                                ELGGXXC 
00655         WHEN PROCESSING-GBAE                                      ELGGXXC 
00656            MOVE WS-BAE-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB)  ELGGXXC 
00657         WHEN PROCESSING-GRPO                                      ELGGXXC 
00658            MOVE WS-RPO-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB)  ELGGXXC 
00659         WHEN PROCESSING-GPPO                                      ELGGXXC 
00660            MOVE WS-PPO-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB)  ELGGXXC 
00661         WHEN PROCESSING-GCPO                                      ELGGXXC 
00662            MOVE WS-CPO-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB)  ELGGXXC 
00663         WHEN PROCESSING-GCBL                                      ELGGXXC 
00664            MOVE WS-CBL-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB)  ELGGXXC 
00665         WHEN PROCESSING-GPAN                                      ELGGXXC 
00666            MOVE WS-PAN-APPLIES TO TCAR-FROM-LINE (TCAR-FROM-SUB)  ELGGXXC 
00667      END-EVALUATE.                                                ELGGXXC 
00668      PERFORM MOVE-TRANSLATION-TO-COMPRESS-A                       ELGGXXC 
00669          VARYING CMF-DESCR-IDX FROM +1 BY +1                      ELGGXXC 
00670                UNTIL   CMF-DESCR-IDX > CMF-NBR-DESCR-LINES.       ELGGXXC 
00671      ADD +1              TO TCAR-FROM-SUB.                        ELGGXXC 
00672      MOVE '.'            TO TCAR-FROM-LINE                        ELGGXXC 
00673          (TCAR-FROM-SUB).                                         ELGGXXC 
00674      PERFORM COMPRESS-AND-PRINT-TEXT-GIVEN.                       ELGGXXC 
00675      PERFORM INSERT-BLANK-LINE.                                   ELGGXXC 
00676                                                                   ELGGXXC 
00677                                                                   ELGGXXC 
00678 ************************************************************      ELGGXXC 
00679 *                                                          *      ELGGXXC 
00680 *        TRANSLATE PPO INDICATOR                           *      ELGGXXC 
00681 * THIS ALSO TRANSLATES RPO INDICATOR AND NOW CPO(03/95)    *      ELGGXXC 
00682 ************************************************************      ELGGXXC 
00683  TRANSLATE-PPO-INDICATOR.                                         ELGGXXC 
00684      INITIALIZE TCAR-FROM-AREA.                                   ELGGXXC 
00685      MOVE 'GROUP'                   TO  CMF-RECORD-PREFIX.        ELGGXXC 
00686      EVALUATE TRUE                                                ELGGXXC 
00687         WHEN PROCESSING-GBAE                                      ELGGXXC 
00688            MOVE 'BAE-INDICATOR' TO CMF-ELEMENT-SYSTEM-NAME        ELGGXXC 
00689         WHEN PROCESSING-GRPO                                      ELGGXXC 
00690            MOVE 'RPO-INDICATOR' TO CMF-ELEMENT-SYSTEM-NAME        ELGGXXC 
00691         WHEN PROCESSING-GPPO                                      ELGGXXC 
00692            MOVE 'PARTICIPAT-PROV-OPTION'                          ELGGXXC 
00693                        TO CMF-ELEMENT-SYSTEM-NAME                 ELGGXXC 
00694         WHEN PROCESSING-GCPO                                      ELGGXXC 
00695            MOVE 'CPO-PARTICIPATION-IND' TO CMF-ELEMENT-SYSTEM-NAMEELGGXXC 
00696         WHEN PROCESSING-GCBL                                      ELGGXXC 
00697            MOVE 'CBL-PARTICIPATION-IND' TO CMF-ELEMENT-SYSTEM-NAMEELGGXXC 
00698         WHEN PROCESSING-GPAN                                      ELGGXXC 
00699            MOVE 'PAN-PARTICIPATION-IND' TO CMF-ELEMENT-SYSTEM-NAMEELGGXXC 
00700      END-EVALUATE.                                                ELGGXXC 
00701      EVALUATE TRUE                                                ELGGXXC 
00702         WHEN PROCESSING-GBAE                                      ELGGXXC 
00703            MOVE WS-GRP-SPC-BAE-IND        TO  CMF-CODE-VALUE      ELGGXXC 
00704         WHEN PROCESSING-GRPO                                      ELGGXXC 
00705            MOVE WS-GRP-SPC-RPO-IND        TO  CMF-CODE-VALUE      ELGGXXC 
00706         WHEN PROCESSING-GPPO                                      ELGGXXC 
00707            MOVE WS-GRP-SPC-PPO-IND        TO  CMF-CODE-VALUE      ELGGXXC 
00708         WHEN PROCESSING-GCPO                                      ELGGXXC 
00709            MOVE WS-GRP-SPC-CPO-IND        TO  CMF-CODE-VALUE      ELGGXXC 
00710         WHEN PROCESSING-GCBL                                      ELGGXXC 
00711            MOVE WS-GRP-SPC-CBL-IND        TO  CMF-CODE-VALUE      ELGGXXC 
00712         WHEN PROCESSING-GPAN                                      ELGGXXC 
00713            MOVE WS-GRP-SPC-PAN-IND        TO  CMF-CODE-VALUE      ELGGXXC 
00714      END-EVALUATE.                                                ELGGXXC 
00715      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELGGXXC 
00716                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGGXXC 
00717      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGXXC 
00718      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00719          ADDRESS OF CMF-DESCR.                                    ELGGXXC 
00720                                                                   ELGGXXC 
00721 ************************************************************      ELGGXXC 
00722 *                                                          *      ELGGXXC 
00723 *        MOVE TRANSLATION TO COMPRESS AREA                 *      ELGGXXC 
00724 *                                                          *      ELGGXXC 
00725 ************************************************************      ELGGXXC 
00726  MOVE-TRANSLATION-TO-COMPRESS-A.                                  ELGGXXC 
00727      ADD +1                  TO TCAR-FROM-SUB.                    ELGGXXC 
00728      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                       ELGGXXC 
00729                                      TCAR-FROM-LINE               ELGGXXC 
00730          (TCAR-FROM-SUB).                                         ELGGXXC 
00731                                                                   ELGGXXC 
00732 ************************************************************      ELGGXXC 
00733 *                                                          *      ELGGXXC 
00734 *        COMPRESS AND PRINT TEXT GIVEN                     *      ELGGXXC 
00735 *                                                          *      ELGGXXC 
00736 ************************************************************      ELGGXXC 
00737  COMPRESS-AND-PRINT-TEXT-GIVEN.                                   ELGGXXC 
00738      PERFORM DO-TEXT-COMPRESSION.                                 ELGGXXC 
00739      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGGXXC 
00740      MOVE +04  TO  TCAR-OUTPUT-FIELD-COUNT.                       ELGGXXC 
00741      MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                        ELGGXXC 
00742                    TCAR-OUTPUT-FIELD-2-LEN                        ELGGXXC 
00743                    TCAR-OUTPUT-FIELD-3-LEN                        ELGGXXC 
00744                    TCAR-OUTPUT-FIELD-4-LEN.                       ELGGXXC 
00745      PERFORM DO-TEXT-UNSTRING.                                    ELGGXXC 
00746      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGXXC 
00747          VARYING WS-SUB FROM 1 BY 1                               ELGGXXC 
00748                UNTIL WS-SUB  GREATER THAN                         ELGGXXC 
00749              TCAR-OUTPUT-FIELDS-USED.                             ELGGXXC 
00750      PERFORM CALL-OUTPUT.                                         ELGGXXC 
00751                                                                   ELGGXXC 
00752                                                                   ELGGXXC 
00753 ************************************************************      ELGGXXC 
00754 *                                                          *      ELGGXXC 
00755 *        PROCESS NORM PROV ACCESS                          *      ELGGXXC 
00756 *                                                          *      ELGGXXC 
00757 ************************************************************      ELGGXXC 
00758  PROCESS-NORM-PROV-ACCESS.                                        ELGGXXC 
00759      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGGXXC 
00760      PERFORM INSERT-BLANK-LINE.                                   ELGGXXC 
00761      PERFORM TRANSLATE-NORM-INCLUDE-EXCLUDE.                      ELGGXXC 
00762      PERFORM LIST-NORM-TABULAR-CONTENTS                           ELGGXXC 
00763          VARYING GSL-INDEX  FROM  1  BY  1                        ELGGXXC 
00764            UNTIL GSL-INDEX  =  GSL-ENTRY-COUNT OR                 ELGGXXC 
00765            GSL-PROVIDER-NO-ARG(GSL-INDEX)  =  HIGH-VALUES.        ELGGXXC 
00766      PERFORM INSERT-BLANK-LINE.                                   ELGGXXC 
00767                                                                   ELGGXXC 
00768 ************************************************************      ELGGXXC 
00769 *                                                          *      ELGGXXC 
00770 *        SEARCH COST CONTAINMENT TABLE                     *      ELGGXXC 
00771 *                                                          *      ELGGXXC 
00772 ************************************************************      ELGGXXC 
00773  SEARCH-COST-CONTAINMENT-TABLE.                                   ELGGXXC 
00774      SET WS-IDX TO 1.                                             ELGGXXC 
00775      SEARCH WS-TAB-TYPE                                           ELGGXXC 
00776            VARYING WS-IDX                                         ELGGXXC 
00777            AT END                                                 ELGGXXC 
00778               SET CIA-AB-TAB-UNDEF  TO  TRUE                      ELGGXXC 
00779               EXEC CICS ABEND                                     ELGGXXC 
00780                         ABCODE(CIA-ABCODE)                        ELGGXXC 
00781               END-EXEC                                            ELGGXXC 
00782            WHEN WS-TAB-TYPE(WS-IDX)  =  SRP-TABULAR-ID            ELGGXXC 
00783                NEXT SENTENCE                                      ELGGXXC 
00784         END-SEARCH.                                               ELGGXXC 
00785                                                                   ELGGXXC 
00786 ************************************************************      ELGGXXC 
00787 *                                                          *      ELGGXXC 
00788 *        READ INTERNAL TABULAR RECORD                      *      ELGGXXC 
00789 *                                                          *      ELGGXXC 
00790 ************************************************************      ELGGXXC 
00791  READ-INTERNAL-TABULAR-RECORD.                                    ELGGXXC 
00792      SET CIA-GCTABULR-DDN  TO TRUE.                               ELGGXXC 
00793      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00794           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELGGXXC 
00795      IF CIA-RC-PTR-NULL                                           ELGGXXC 
00796          PERFORM GETMAIN-IO-PARM-AREA.                            ELGGXXC 
00797      SET CIA-GCTABULR-DDN  TO TRUE.                               ELGGXXC 
00798      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00799           ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                 ELGGXXC 
00800      SET IOP-RD                                                   ELGGXXC 
00801          IOP-FCQ-NONE                                             ELGGXXC 
00802          IOP-KVQ-EQ                                               ELGGXXC 
00803          IOP-STG-MODE-MOVE  TO  TRUE.                             ELGGXXC 
00804      MOVE SRP-INTERNAL-TAB  TO  KWA-GCTABULR-KEY                  ELGGXXC 
00805                             IOP-FILE-KEY.                         ELGGXXC 
00806      MOVE SPACES  TO  IOP-AIX-DDNAME.                             ELGGXXC 
00807      SET IOP-REC-PTR  TO  NULL.                                   ELGGXXC 
00808      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELGGXXC 
00809                       COMMAREA(DFHCOMMAREA) END-EXEC.             ELGGXXC 
00810      IF NOT IOP-RC-OK                                             ELGGXXC 
00811          PERFORM TABULAR-NOT-FOUND.                               ELGGXXC 
00812      EVALUATE TRUE                                                ELGGXXC 
00813         WHEN SRP-TABULAR-ID  =  WS-GPPO OR WS-GRPO OR WS-GCPO     ELGGXXC 
00814                     OR WS-GCBL OR WS-GPAN                         ELGGXXC 
00815            PERFORM ADDRESS-GPPO-TABULAR-AREA                      ELGGXXC 
00816         WHEN OTHER                                                ELGGXXC 
00817             PERFORM ADDRESS-NORM-PROV-TABULAR-AREA                ELGGXXC 
00818      END-EVALUATE.                                                ELGGXXC 
00819                                                                   ELGGXXC 
00820 ************************************************************      ELGGXXC 
00821 *                                                          *      ELGGXXC 
00822 *        GETMAIN IO PARM AREA                              *      ELGGXXC 
00823 *                                                          *      ELGGXXC 
00824 ************************************************************      ELGGXXC 
00825  GETMAIN-IO-PARM-AREA.                                            ELGGXXC 
00826      SET CIA-STG-GETMAIN  TO  TRUE.                               ELGGXXC 
00827      EXEC CICS  LINK  PROGRAM('ELUSTGMG')                         ELGGXXC 
00828                       COMMAREA(DFHCOMMAREA)   END-EXEC.           ELGGXXC 
00829      SET WS-DID-GETMAIN TO TRUE.                                  ELGGXXC 
00830                                                                   ELGGXXC 
00831 ************************************************************      ELGGXXC 
00832 *                                                          *      ELGGXXC 
00833 *        ADDRESS NORM PROV TABULAR AREA                    *      ELGGXXC 
00834 *                                                          *      ELGGXXC 
00835 ************************************************************      ELGGXXC 
00836  ADDRESS-NORM-PROV-TABULAR-AREA.                                  ELGGXXC 
00837      SET ADDRESS OF GSL-TABULAR-REC-AREA  TO                      ELGGXXC 
00838          IOP-REC-PTR.                                             ELGGXXC 
00839      IF GSL-PROVIDER-NO-ARG(1)  =  HIGH-VALUES                    ELGGXXC 
00840          PERFORM EMPTY-COST-CONTAINMENT-TAB.                      ELGGXXC 
00841                                                                   ELGGXXC 
00842 ************************************************************      ELGGXXC 
00843 *                                                          *      ELGGXXC 
00844 *        ADDRESS GPPO TABULAR AREA                         *      ELGGXXC 
00845 *                                                          *      ELGGXXC 
00846 ************************************************************      ELGGXXC 
00847  ADDRESS-GPPO-TABULAR-AREA.                                       ELGGXXC 
00848      SET ADDRESS OF GSW-TABULAR-REC-AREA  TO                      ELGGXXC 
00849          IOP-REC-PTR.                                             ELGGXXC 
00850      IF GSW-PROVIDER-NO-ARG(1)  =  HIGH-VALUES                    ELGGXXC 
00851          PERFORM EMPTY-COST-CONTAINMENT-TAB.                      ELGGXXC 
00852                                                                   ELGGXXC 
00853                                                                   ELGGXXC 
00854 ************************************************************      ELGGXXC 
00855 *                                                          *      ELGGXXC 
00856 *        TRANSLATE NORM INCLUDE-EXCLUDE IND                *      ELGGXXC 
00857 *                                                          *      ELGGXXC 
00858 ************************************************************      ELGGXXC 
00859  TRANSLATE-NORM-INCLUDE-EXCLUDE.                                  ELGGXXC 
00860      INITIALIZE TCAR-FROM-AREA.                                   ELGGXXC 
00861      MOVE SRP-TABULAR-ID  TO  CMF-RECORD-PREFIX.                  ELGGXXC 
00862      MOVE 'INCLUDE-EXCLUDE-IND'  TO                               ELGGXXC 
00863          CMF-ELEMENT-SYSTEM-NAME.                                 ELGGXXC 
00864      MOVE GSL-INCLUDE-EXCLUDE-IND  TO  CMF-CODE-VALUE.            ELGGXXC 
00865      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELGGXXC 
00866                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGGXXC 
00867      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGXXC 
00868      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00869          CMF-DESCR.                                               ELGGXXC 
00870      PERFORM STRING-INCLUDE-EXCLUDE-IND.                          ELGGXXC 
00871                                                                   ELGGXXC 
00872 ************************************************************      ELGGXXC 
00873 *                                                          *      ELGGXXC 
00874 *        STRING INCLUDE-EXCLUDE IND                        *      ELGGXXC 
00875 *                                                          *      ELGGXXC 
00876 ************************************************************      ELGGXXC 
00877  STRING-INCLUDE-EXCLUDE-IND.                                      ELGGXXC 
00878      STRING 'THIS ',  SRP-CCP-NAME DELIMITED BY '  '              ELGGXXC 
00879             ' PROGRAM '  DELIMITED BY SIZE                        ELGGXXC 
00880             CMF-DESCR-LINE(1)  DELIMITED BY '  '                  ELGGXXC 
00881             ' THE FOLLOWING PROVIDERS:'  DELIMITED BY             ELGGXXC 
00882          SIZE                                                     ELGGXXC 
00883             INTO TCAR-FROM-AREA.                                  ELGGXXC 
00884      PERFORM DO-TEXT-COMPRESSION.                                 ELGGXXC 
00885      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELGGXXC 
00886      MOVE +04  TO  TCAR-OUTPUT-FIELD-COUNT.                       ELGGXXC 
00887      MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                        ELGGXXC 
00888                    TCAR-OUTPUT-FIELD-2-LEN                        ELGGXXC 
00889                    TCAR-OUTPUT-FIELD-3-LEN                        ELGGXXC 
00890                    TCAR-OUTPUT-FIELD-4-LEN.                       ELGGXXC 
00891      PERFORM DO-TEXT-UNSTRING.                                    ELGGXXC 
00892      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGXXC 
00893          VARYING WS-SUB  FROM  1  BY  1                           ELGGXXC 
00894                UNTIL WS-SUB  GREATER THAN                         ELGGXXC 
00895              TCAR-OUTPUT-FIELDS-USED.                             ELGGXXC 
00896      PERFORM CALL-OUTPUT.                                         ELGGXXC 
00897                                                                   ELGGXXC 
00898 ************************************************************      ELGGXXC 
00899 *                                                          *      ELGGXXC 
00900 *        LIST NORM TABULAR CONTENTS                        *      ELGGXXC 
00901 *                                                          *      ELGGXXC 
00902 ************************************************************      ELGGXXC 
00903  LIST-NORM-TABULAR-CONTENTS.                                      ELGGXXC 
00904      SET PROCESSING-NORM-TABULAR TO TRUE.                         ELGGXXC 
00905      MOVE GSL-PROVIDER-NO-ARG(GSL-INDEX)  TO                      ELGGXXC 
00906          PDB-I-PROVIDER-NBR.                                      ELGGXXC 
00907      MOVE 'PRVDR'  TO  PDB-I-REQUEST-TYPE-N.                      ELGGXXC 
00908      MOVE '02'  TO  PDB-I-VERSION.                                ELGGXXC 
00909      MOVE 'D'  TO  PDB-I-ACCESS-MODE.                             ELGGXXC 
00910      SET PDB-I-SS-ACTIVE-INACTIVE-PROV TO TRUE.                   ELGGXXC 
00911      SET PDB-I-UA-UNLIMITED-ACCESS TO TRUE.                       ELGGXXC 
00912      EXEC CICS  LINK  PROGRAM('DBPIOC') COMMAREA(PDB-IO-AREA)     ELGGXXC 
00913                       END-EXEC.                                   ELGGXXC 
00914      IF PDB-O-RC-SUCCESSFUL                                       ELGGXXC 
00915         PERFORM STRING-PROVIDER-NAME-AND-TYPE                     ELGGXXC 
00916      ELSE                                                         ELGGXXC 
00917        IF NOT PDB-O-RC-SUCCESSFUL                                 ELGGXXC 
00918          PERFORM CHECK-CM-FOR-PROV-NAME                           ELGGXXC 
00919          IF NOT CMF-RC-OK                                         ELGGXXC 
00920              PERFORM PROVIDER-FILE-PROBLEM                        ELGGXXC 
00921          END-IF                                                   ELGGXXC 
00922        END-IF                                                     ELGGXXC 
00923      END-IF.                                                      ELGGXXC 
00924      PERFORM CALL-OUTPUT.                                         ELGGXXC 
00925                                                                   ELGGXXC 
00926 ************************************************************      ELGGXXC 
00927 *                                                          *      ELGGXXC 
00928 *       CHECK CODES MANUAL FOR PROVIDER NAME               *      ELGGXXC 
00929 *                                                          *      ELGGXXC 
00930 ************************************************************      ELGGXXC 
00931  CHECK-CM-FOR-PROV-NAME.                                          ELGGXXC 
00932      MOVE SRP-TABULAR-ID TO CMF-RECORD-PREFIX.                    ELGGXXC 
00933      MOVE 'PROVIDER-NO-ARG' TO CMF-ELEMENT-SYSTEM-NAME.           ELGGXXC 
00934      SET PDB-I-SS-ACTIVE-INACTIVE-PROV TO TRUE.                   ELGGXXC 
00935      IF PROCESSING-NORM-TABULAR                                   ELGGXXC 
00936         MOVE GSL-PROVIDER-NO-ARG(GSL-INDEX) TO                    ELGGXXC 
00937             CMF-CODE-VALUE                                        ELGGXXC 
00938      ELSE                                                         ELGGXXC 
00939         MOVE GSW-PROVIDER-NO-ARG(GSW-INDEX) TO                    ELGGXXC 
00940             CMF-CODE-VALUE.                                       ELGGXXC 
00941      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELGGXXC 
00942          COMMAREA(DFHCOMMAREA)                                    ELGGXXC 
00943                       END-EXEC.                                   ELGGXXC 
00944      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGXXC 
00945      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
00946          ADDRESS OF CMF-DESCR.                                    ELGGXXC 
00947      IF CMF-RC-OK                                                 ELGGXXC 
00948         PERFORM PROCESS-CM-PROVIDER-INFO                          ELGGXXC 
00949      ELSE                                                         ELGGXXC 
00950         NEXT SENTENCE.                                            ELGGXXC 
00951 *                                                                 ELGGXXC 
00952 ************************************************************      ELGGXXC 
00953 *                                                          *      ELGGXXC 
00954 *    PROCESS CODES MANUAL INFORMATION                      *      ELGGXXC 
00955 *                                                          *      ELGGXXC 
00956 ************************************************************      ELGGXXC 
00957  PROCESS-CM-PROVIDER-INFO.                                        ELGGXXC 
00958      MOVE 1 TO TCAR-FROM-SUB.                                     ELGGXXC 
00959      IF PROCESSING-NORM-TABULAR                                   ELGGXXC 
00960         MOVE GSL-PROVIDER-NO-ARG(GSL-INDEX) TO                    ELGGXXC 
00961             TCAR-FROM-LINE(TCAR-FROM-SUB)                         ELGGXXC 
00962      ELSE                                                         ELGGXXC 
00963         MOVE GSW-PROVIDER-NO-ARG(GSW-INDEX) TO                    ELGGXXC 
00964             TCAR-FROM-LINE(TCAR-FROM-SUB).                        ELGGXXC 
00965      PERFORM WITH TEST BEFORE VARYING                             ELGGXXC 
00966          WS-CM-SUB FROM 1 BY 1                                    ELGGXXC 
00967              UNTIL TCAR-FROM-SUB > CMF-NBR-DESCR-LINES            ELGGXXC 
00968               ADD 1 TO TCAR-FROM-SUB                              ELGGXXC 
00969               MOVE CMF-DESCR-LINE (WS-CM-SUB) TO                  ELGGXXC 
00970                  TCAR-FROM-LINE (TCAR-FROM-SUB)                   ELGGXXC 
00971      END-PERFORM.                                                 ELGGXXC 
00972      PERFORM DO-TEXT-COMPRESSION.                                 ELGGXXC 
00973      MOVE +74  TO  TCAR-OUTPUT-FIELD-1-LEN.                       ELGGXXC 
00974      MOVE +63  TO  TCAR-OUTPUT-FIELD-2-LEN                        ELGGXXC 
00975                    TCAR-OUTPUT-FIELD-3-LEN                        ELGGXXC 
00976                    TCAR-OUTPUT-FIELD-4-LEN.                       ELGGXXC 
00977      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELGGXXC 
00978      PERFORM DO-TEXT-UNSTRING.                                    ELGGXXC 
00979      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGXXC 
00980          VARYING WS-SUB FROM 1 BY 1                               ELGGXXC 
00981                UNTIL   WS-SUB GREATER THAN                        ELGGXXC 
00982              TCAR-OUTPUT-FIELDS-USED.                             ELGGXXC 
00983                                                                   ELGGXXC 
00984 ************************************************************      ELGGXXC 
00985 *                                                          *      ELGGXXC 
00986 *        STRING PROVIDER NAME AND TYPE                     *      ELGGXXC 
00987 *                                                          *      ELGGXXC 
00988 ************************************************************      ELGGXXC 
00989  STRING-PROVIDER-NAME-AND-TYPE.                                   ELGGXXC 
00990      SET ADDRESS OF PROVIDER-MSTR-REC  TO                         ELGGXXC 
00991                                      ADDRESS OF                   ELGGXXC 
00992          PDB-O-RECORD-AREA.                                       ELGGXXC 
00993      STRING GSL-PROVIDER-NO-ARG(GSL-INDEX),                       ELGGXXC 
00994             ' ',  DELIMITED BY SIZE,                              ELGGXXC 
00995             PFM-PAYEE-NAME-1,  DELIMITED BY '  ',                 ELGGXXC 
00996             ' ',  DELIMITED BY SIZE,                              ELGGXXC 
00997             PFM-PAYEE-NAME-2,   ','    DELIMITED BY ' ',          ELGGXXC 
00998             INTO  TCAR-FROM-AREA.                                 ELGGXXC 
00999      PERFORM TRANSLATE-PROVIDER-CODE.                             ELGGXXC 
01000                                                                   ELGGXXC 
01001 ************************************************************      ELGGXXC 
01002 *                                                          *      ELGGXXC 
01003 *        PROCESS GPPO PROV ACCESS                          *      ELGGXXC 
01004 *                                                          *      ELGGXXC 
01005 ************************************************************      ELGGXXC 
01006  PROCESS-GPPO-PROV-ACCESS.                                        ELGGXXC 
01007      PERFORM READ-INTERNAL-TABULAR-RECORD.                        ELGGXXC 
01008      IF GSW-PROVIDER-NO-ARG(1)  NOT =  HIGH-VALUES AND            ELGGXXC 
01009             SRP-TABULAR-SLOT-NO > +0                              ELGGXXC 
01010          PERFORM SAVE-GPPOS-FIRST-I-E-IND                         ELGGXXC 
01011      ELSE                                                         ELGGXXC 
01012          PERFORM EMPTY-COST-CONTAINMENT-TAB.                      ELGGXXC 
01013      PERFORM SEARCH-GPPO-TABLE-FOR-FIRST                          ELGGXXC 
01014          VARYING GSW-INDEX  FROM  1  BY  1                        ELGGXXC 
01015            UNTIL GSW-INDEX  =  GSW-ENTRY-COUNT OR                 ELGGXXC 
01016            GSW-PROVIDER-NO-ARG(GSW-INDEX)  =  HIGH-VALUES.        ELGGXXC 
01017      PERFORM INSERT-BLANK-LINE.                                   ELGGXXC 
01018      MOVE 'Y'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXC 
01019      PERFORM SEARCH-GPPO-TABLE-FOR-OTHER-TH                       ELGGXXC 
01020          VARYING GSW-INDEX  FROM  1  BY  1                        ELGGXXC 
01021            UNTIL GSW-INDEX  =  GSW-ENTRY-COUNT OR                 ELGGXXC 
01022            GSW-PROVIDER-NO-ARG(GSW-INDEX)  =  HIGH-VALUES.        ELGGXXC 
01023      PERFORM INSERT-BLANK-LINE.                                   ELGGXXC 
01024                                                                   ELGGXXC 
01025                                                                   ELGGXXC 
01026 ************************************************************      ELGGXXC 
01027 *                                                          *      ELGGXXC 
01028 *        SAVE GPPOS FIRST I-E IND                          *      ELGGXXC 
01029 *                                                          *      ELGGXXC 
01030 ************************************************************      ELGGXXC 
01031  SAVE-GPPOS-FIRST-I-E-IND.                                        ELGGXXC 
01032      MOVE GSW-INCLUDE-EXCLUDE-IND(1)  TO  WS-I-E-IND.             ELGGXXC 
01033                                                                   ELGGXXC 
01034 ************************************************************      ELGGXXC 
01035 *                                                          *      ELGGXXC 
01036 *        SEARCH GPPO TABLE FOR FIRST                       *      ELGGXXC 
01037 *                                                          *      ELGGXXC 
01038 ************************************************************      ELGGXXC 
01039  SEARCH-GPPO-TABLE-FOR-FIRST.                                     ELGGXXC 
01040      IF GSW-INCLUDE-EXCLUDE-IND(GSW-INDEX)  =                     ELGGXXC 
01041          WS-I-E-IND                                               ELGGXXC 
01042          PERFORM LIST-GPPO-TABULAR-CONTENTS.                      ELGGXXC 
01043                                                                   ELGGXXC 
01044 ************************************************************      ELGGXXC 
01045 *                                                          *      ELGGXXC 
01046 *        SEARCH GPPO TABLE FOR OTHER THAN FIRST            *      ELGGXXC 
01047 *                                                          *      ELGGXXC 
01048 ************************************************************      ELGGXXC 
01049  SEARCH-GPPO-TABLE-FOR-OTHER-TH.                                  ELGGXXC 
01050      IF GSW-INCLUDE-EXCLUDE-IND(GSW-INDEX)  NOT =                 ELGGXXC 
01051          WS-I-E-IND                                               ELGGXXC 
01052          PERFORM LIST-GPPO-TABULAR-CONTENTS.                      ELGGXXC 
01053                                                                   ELGGXXC 
01054 ************************************************************      ELGGXXC 
01055 *                                                          *      ELGGXXC 
01056 *        LIST GPPO TABULAR CONTENTS                        *      ELGGXXC 
01057 *                                                          *      ELGGXXC 
01058 ************************************************************      ELGGXXC 
01059  LIST-GPPO-TABULAR-CONTENTS.                                      ELGGXXC 
01060      INITIALIZE WS-PROCESSING-SWITCH.                             ELGGXXC 
01061      IF WS-FIRST-TIME-THIS-IND                                    ELGGXXC 
01062          PERFORM DETERMINE-IF-CONTRACT-OVERRIDE.                  ELGGXXC 
01063      MOVE GSW-PROVIDER-NO-ARG(GSW-INDEX)  TO                      ELGGXXC 
01064          PDB-I-PROVIDER-NBR.                                      ELGGXXC 
01065      MOVE 'PRVDR'  TO  PDB-I-REQUEST-TYPE-N.                      ELGGXXC 
01066      MOVE '02'  TO  PDB-I-VERSION.                                ELGGXXC 
01067      MOVE 'D'  TO  PDB-I-ACCESS-MODE.                             ELGGXXC 
01068      SET PDB-I-UA-UNLIMITED-ACCESS TO TRUE.                       ELGGXXC 
01069      SET PDB-I-SS-ACTIVE-INACTIVE-PROV TO TRUE.                   ELGGXXC 
01070      EXEC CICS  LINK  PROGRAM('DBPIOC') COMMAREA(PDB-IO-AREA)     ELGGXXC 
01071                         END-EXEC.                                 ELGGXXC 
01072      IF PDB-O-RC-SUCCESSFUL                                       ELGGXXC 
01073          PERFORM STRING-GPPO-PROVIDER-NAME-ANDX                   ELGGXXC 
01074      ELSE                                                         ELGGXXC 
01075      IF NOT PDB-O-RC-SUCCESSFUL                                   ELGGXXC 
01076          PERFORM CHECK-CM-FOR-PROV-NAME                           ELGGXXC 
01077          IF NOT CMF-RC-OK                                         ELGGXXC 
01078             PERFORM PROVIDER-FILE-PROBLEM                         ELGGXXC 
01079          END-IF                                                   ELGGXXC 
01080      END-IF                                                       ELGGXXC 
01081      END-IF.                                                      ELGGXXC 
01082      PERFORM CALL-OUTPUT.                                         ELGGXXC 
01083                                                                   ELGGXXC 
01084 ************************************************************      ELGGXXC 
01085 *                                                          *      ELGGXXC 
01086 *        DETERMINE IF CONTRACT OVERRIDES CORPORATE LIST    *      ELGGXXC 
01087 *                                                          *      ELGGXXC 
01088 ************************************************************      ELGGXXC 
01089  DETERMINE-IF-CONTRACT-OVERRIDE.                                  ELGGXXC 
01090      IF PROCESSING-GCBL OR PROCESSING-GPAN                        ELGGXXC 
01091         PERFORM DISPLAY-CONTRACT-USES-CORPORAT                    ELGGXXC 
01092      ELSE                                                         ELGGXXC 
01093         EVALUATE TRUE                                             ELGGXXC 
01094            WHEN GSW-CORP-LIST-OVERIDE-IND = 'N'                   ELGGXXC 
01095                PERFORM DISPLAY-CONTRACT-USES-CORPORAT             ELGGXXC 
01096            WHEN OTHER                                             ELGGXXC 
01097                PERFORM DISPLAY-EXCEPTION-FROM-CORPORA             ELGGXXC 
01098         END-EVALUATE                                              ELGGXXC 
01099      END-IF.                                                      ELGGXXC 
01100      MOVE 'N'  TO  WS-FIRST-TIME-IND-VAL.                         ELGGXXC 
01101                                                                   ELGGXXC 
01102 ************************************************************      ELGGXXC 
01103 *                                                          *      ELGGXXC 
01104 *        DISPLAY CONTRACT USES CORPORATE LIST              *      ELGGXXC 
01105 *                                                          *      ELGGXXC 
01106 ************************************************************      ELGGXXC 
01107  DISPLAY-CONTRACT-USES-CORPORAT.                                  ELGGXXC 
01108      PERFORM DO-INITIALIZE-TEXT-COMPRESSION.                      ELGGXXC 
01109      PERFORM TRANSLATE-GPPO-INCLUDE-EXCLUDE.                      ELGGXXC 
01110      EVALUATE TRUE                                                ELGGXXC 
01111         WHEN PROCESSING-GCBL                                      ELGGXXC 
01112            STRING WS-CBL-PHRASE                DELIMITED BY       ELGGXXC 
01113                SIZE                                               ELGGXXC 
01114                   CMF-DESCR-LINE(1)            DELIMITED BY '  '  ELGGXXC 
01115                   ' THE FOLLOWING PROVIDERS:'  DELIMITED BY       ELGGXXC 
01116                SIZE                                               ELGGXXC 
01117                   INTO TCAR-FROM-AREA                             ELGGXXC 
01118         WHEN PROCESSING-GPAN                                      ELGGXXC 
01119            STRING WS-PAN-PHRASE                DELIMITED BY       ELGGXXC 
01120                SIZE                                               ELGGXXC 
01121                   CMF-DESCR-LINE(1)            DELIMITED BY '  '  ELGGXXC 
01122                   ' THE FOLLOWING PROVIDERS:'  DELIMITED BY       ELGGXXC 
01123                SIZE                                               ELGGXXC 
01124                   INTO TCAR-FROM-AREA                             ELGGXXC 
01125         WHEN PROCESSING-GBAE                                      ELGGXXC 
01126            STRING WS-BAE-PHRASE                DELIMITED BY       ELGGXXC 
01127                SIZE                                               ELGGXXC 
01128                   CMF-DESCR-LINE(1)            DELIMITED BY '  '  ELGGXXC 
01129                   ' THE FOLLOWING PROVIDERS:'  DELIMITED BY       ELGGXXC 
01130                SIZE                                               ELGGXXC 
01131                   INTO TCAR-FROM-AREA                             ELGGXXC 
01132         WHEN PROCESSING-GRPO                                      ELGGXXC 
01133            STRING WS-RPO-PHRASE                DELIMITED BY       ELGGXXC 
01134                SIZE                                               ELGGXXC 
01135                   CMF-DESCR-LINE(1)            DELIMITED BY '  '  ELGGXXC 
01136                   ' THE FOLLOWING PROVIDERS:'  DELIMITED BY       ELGGXXC 
01137                SIZE                                               ELGGXXC 
01138                   INTO TCAR-FROM-AREA                             ELGGXXC 
01139         WHEN PROCESSING-GPPO                                      ELGGXXC 
01140            STRING WS-CORP-LIST                 DELIMITED BY       ELGGXXC 
01141                SIZE                                               ELGGXXC 
01142                   CMF-DESCR-LINE(1)            DELIMITED BY '  '  ELGGXXC 
01143                   ' THE FOLLOWING PROVIDERS:'  DELIMITED BY       ELGGXXC 
01144                SIZE                                               ELGGXXC 
01145                  INTO TCAR-FROM-AREA                              ELGGXXC 
01146         WHEN PROCESSING-GCPO                                      ELGGXXC 
01147            STRING WS-CPO-PHRASE              DELIMITED BY         ELGGXXC 
01148                SIZE                                               ELGGXXC 
01149                   CMF-DESCR-LINE(1)            DELIMITED BY '  '  ELGGXXC 
01150                   ' THE FOLLOWING PROVIDERS:'  DELIMITED BY       ELGGXXC 
01151                SIZE                                               ELGGXXC 
01152                  INTO TCAR-FROM-AREA                              ELGGXXC 
01153      END-EVALUATE.                                                ELGGXXC 
01154      PERFORM COMPRESS-AND-PRINT-TEXT-GIVEN.                       ELGGXXC 
01155                                                                   ELGGXXC 
01156 ************************************************************      ELGGXXC 
01157 *                                                          *      ELGGXXC 
01158 *        DISPLAY EXCEPTION FROM CORPORATE LIST             *      ELGGXXC 
01159 * CPO, CBL AND PAN ARE NOT HERE AS WE WILL DISPLAY THEIR   *      ELGGXXC 
01160 * RESPECTIVE TABULARS WHEREVER THEY EXIST.                 *      ELGGXXC 
01161 ************************************************************      ELGGXXC 
01162  DISPLAY-EXCEPTION-FROM-CORPORA.                                  ELGGXXC 
01163      EVALUATE TRUE                                                ELGGXXC 
01164         WHEN PROCESSING-GBAE                                      ELGGXXC 
01165            ADD +1        TO COF-NBR-DTL-LINES                     ELGGXXC 
01166            MOVE WS-DIFFER-LIST-E TO COF-DTL-LINE                  ELGGXXC 
01167               (COF-NBR-DTL-LINES)                                 ELGGXXC 
01168            ADD +1        TO COF-NBR-DTL-LINES                     ELGGXXC 
01169            MOVE WS-DIFFER-LIST-E1 TO COF-DTL-LINE                 ELGGXXC 
01170                (COF-NBR-DTL-LINES)                                ELGGXXC 
01171            PERFORM CALL-OUTPUT                                    ELGGXXC 
01172         WHEN PROCESSING-GRPO                                      ELGGXXC 
01173            ADD +1        TO COF-NBR-DTL-LINES                     ELGGXXC 
01174            MOVE WS-DIFFER-LIST-A TO COF-DTL-LINE                  ELGGXXC 
01175               (COF-NBR-DTL-LINES)                                 ELGGXXC 
01176            ADD +1        TO COF-NBR-DTL-LINES                     ELGGXXC 
01177            MOVE WS-DIFFER-LIST-B TO COF-DTL-LINE                  ELGGXXC 
01178                (COF-NBR-DTL-LINES)                                ELGGXXC 
01179            PERFORM CALL-OUTPUT                                    ELGGXXC 
01180         WHEN PROCESSING-GPPO                                      ELGGXXC 
01181            ADD +1                     TO COF-NBR-DTL-LINES        ELGGXXC 
01182            MOVE WS-DIFFER-CORP-LIST-A TO COF-DTL-LINE             ELGGXXC 
01183                (COF-NBR-DTL-LINES)                                ELGGXXC 
01184            ADD +1                     TO COF-NBR-DTL-LINES        ELGGXXC 
01185            MOVE WS-DIFFER-CORP-LIST-B TO COF-DTL-LINE             ELGGXXC 
01186               (COF-NBR-DTL-LINES)                                 ELGGXXC 
01187            PERFORM CALL-OUTPUT                                    ELGGXXC 
01188      END-EVALUATE.                                                ELGGXXC 
01189                                                                   ELGGXXC 
01190 ************************************************************      ELGGXXC 
01191 *                                                          *      ELGGXXC 
01192 *        TRANSLATE GPPO INCLUDE-EXCLUDE IND                *      ELGGXXC 
01193 *  THIS ALSO TRANSLATES GRPO I/E IND                       *      ELGGXXC 
01194 ************************************************************      ELGGXXC 
01195  TRANSLATE-GPPO-INCLUDE-EXCLUDE.                                  ELGGXXC 
01196      MOVE SRP-TABULAR-ID  TO  CMF-RECORD-PREFIX.                  ELGGXXC 
01197      MOVE 'INCLUDE-EXCLUDE-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.     ELGGXXC 
01198      MOVE GSW-INCLUDE-EXCLUDE-IND(GSW-INDEX)                      ELGGXXC 
01199                  TO  CMF-CODE-VALUE.                              ELGGXXC 
01200      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELGGXXC 
01201                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGGXXC 
01202      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGXXC 
01203      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
01204          ADDRESS OF CMF-DESCR.                                    ELGGXXC 
01205                                                                   ELGGXXC 
01206 ************************************************************      ELGGXXC 
01207 *                                                          *      ELGGXXC 
01208 *        MOVE COMPRESSED PHRASE                            *      ELGGXXC 
01209 *                                                          *      ELGGXXC 
01210 ************************************************************      ELGGXXC 
01211  MOVE-COMPRESSED-PHRASE.                                          ELGGXXC 
01212      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXC 
01213      MOVE TCAR-OPF-DATA(WS-SUB)  TO                               ELGGXXC 
01214          COF-DTL-LINE(COF-NBR-DTL-LINES).                         ELGGXXC 
01215                                                                   ELGGXXC 
01216 ************************************************************      ELGGXXC 
01217 *                                                          *      ELGGXXC 
01218 *        STRING GPPO PROVIDER NAME AND TYPE                *      ELGGXXC 
01219 *                                                          *      ELGGXXC 
01220 ************************************************************      ELGGXXC 
01221  STRING-GPPO-PROVIDER-NAME-ANDX.                                  ELGGXXC 
01222      SET ADDRESS OF PROVIDER-MSTR-REC  TO                         ELGGXXC 
01223                                      ADDRESS OF                   ELGGXXC 
01224          PDB-O-RECORD-AREA.                                       ELGGXXC 
01225      STRING GSW-PROVIDER-NO-ARG(GSW-INDEX),                       ELGGXXC 
01226             ' ',  DELIMITED BY SIZE,                              ELGGXXC 
01227             PFM-PAYEE-NAME-1,  DELIMITED BY '  ',                 ELGGXXC 
01228             ' ',  DELIMITED BY SIZE,                              ELGGXXC 
01229             PFM-PAYEE-NAME-2,   ','    DELIMITED BY ' ',          ELGGXXC 
01230             INTO  TCAR-FROM-AREA.                                 ELGGXXC 
01231      PERFORM TRANSLATE-PROVIDER-CODE.                             ELGGXXC 
01232                                                                   ELGGXXC 
01233 ************************************************************      ELGGXXC 
01234 *                                                          *      ELGGXXC 
01235 *        TRANSLATE PROVIDER CODE                           *      ELGGXXC 
01236 *                                                          *      ELGGXXC 
01237 ************************************************************      ELGGXXC 
01238  TRANSLATE-PROVIDER-CODE.                                         ELGGXXC 
01239      MOVE WS-PVE  TO  CMF-RECORD-PREFIX.                          ELGGXXC 
01240      MOVE 'PROVIDER-CODE'  TO  CMF-ELEMENT-SYSTEM-NAME.           ELGGXXC 
01241      MOVE PFM-PROVIDER-TYPE  TO  CMF-CODE-VALUE.                  ELGGXXC 
01242      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELGGXXC 
01243          COMMAREA(DFHCOMMAREA)                                    ELGGXXC 
01244                       END-EXEC.                                   ELGGXXC 
01245      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGGXXC 
01246      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGGXXC 
01247          ADDRESS OF CMF-DESCR.                                    ELGGXXC 
01248      PERFORM MOVE-CMF-DESCR-FOR-COMPRESS                          ELGGXXC 
01249          VARYING WS-SUB  FROM  1  BY  1                           ELGGXXC 
01250             UNTIL WS-SUB  >  CMF-NBR-DESCR-LINES.                 ELGGXXC 
01251      PERFORM DO-TEXT-COMPRESSION.                                 ELGGXXC 
01252      MOVE +74  TO  TCAR-OUTPUT-FIELD-1-LEN.                       ELGGXXC 
01253      MOVE +63  TO  TCAR-OUTPUT-FIELD-2-LEN                        ELGGXXC 
01254                    TCAR-OUTPUT-FIELD-3-LEN                        ELGGXXC 
01255                    TCAR-OUTPUT-FIELD-4-LEN.                       ELGGXXC 
01256      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELGGXXC 
01257      PERFORM DO-TEXT-UNSTRING.                                    ELGGXXC 
01258      PERFORM MOVE-COMPRESSED-PHRASE                               ELGGXXC 
01259          VARYING WS-SUB FROM 1 BY 1                               ELGGXXC 
01260                UNTIL   WS-SUB GREATER THAN                        ELGGXXC 
01261              TCAR-OUTPUT-FIELDS-USED.                             ELGGXXC 
01262                                                                   ELGGXXC 
01263 ************************************************************      ELGGXXC 
01264 *                                                          *      ELGGXXC 
01265 *        MOVE CMF DESCR FOR COMPRESS                       *      ELGGXXC 
01266 *                                                          *      ELGGXXC 
01267 ************************************************************      ELGGXXC 
01268  MOVE-CMF-DESCR-FOR-COMPRESS.                                     ELGGXXC 
01269      COMPUTE  TCAR-FROM-SUB  =  WS-SUB  +  2.                     ELGGXXC 
01270      MOVE CMF-DESCR-LINE(WS-SUB)  TO                              ELGGXXC 
01271          TCAR-FROM-LINE(TCAR-FROM-SUB).                           ELGGXXC 
01272                                                                   ELGGXXC 
01273 ************************************************************      ELGGXXC 
01274 *                                                          *      ELGGXXC 
01275 *        INSERT BLANK LINE                                 *      ELGGXXC 
01276 *                                                          *      ELGGXXC 
01277 ************************************************************      ELGGXXC 
01278  INSERT-BLANK-LINE.                                               ELGGXXC 
01279      ADD  1  TO  COF-NBR-DTL-LINES.                               ELGGXXC 
01280      MOVE SPACES  TO  COF-DTL-LINE(COF-NBR-DTL-LINES).            ELGGXXC 
01281      PERFORM CALL-OUTPUT.                                         ELGGXXC 
01282                                                                   ELGGXXC 
01283 ************************************************************      ELGGXXC 
01284 *                                                          *      ELGGXXC 
01285 *        DO INITIALIZE TEXT COMPRESSION                    *      ELGGXXC 
01286 *                                                          *      ELGGXXC 
01287 ************************************************************      ELGGXXC 
01288  DO-INITIALIZE-TEXT-COMPRESSION.                                  ELGGXXC 
01289      INITIALIZE TCAR-FROM-LENGTH                                  ELGGXXC 
01290                 TCAR-AREA-LENGTH                                  ELGGXXC 
01291                 TCAR-FROM-SUB.                                    ELGGXXC 
01292                                                                   ELGGXXC 
01293 ************************************************************      ELGGXXC 
01294 *                                                          *      ELGGXXC 
01295 *        DO TEXT COMPRESSION                               *      ELGGXXC 
01296 *                                                          *      ELGGXXC 
01297 ************************************************************      ELGGXXC 
01298  DO-TEXT-COMPRESSION.                                             ELGGXXC 
01299      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGGXXC 
01300                                                                   ELGGXXC 
01301 ************************************************************      ELGGXXC 
01302 *                                                          *      ELGGXXC 
01303 *        DO TEXT UNSTRING                                  *      ELGGXXC 
01304 *                                                          *      ELGGXXC 
01305 ************************************************************      ELGGXXC 
01306  DO-TEXT-UNSTRING.                                                ELGGXXC 
01307      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGGXXC 
01308                                                                   ELGGXXC 
01309 ************************************************************      ELGGXXC 
01310 *                                                          *      ELGGXXC 
01311 *        CALL OUTPUT                                       *      ELGGXXC 
01312 *                                                          *      ELGGXXC 
01313 ************************************************************      ELGGXXC 
01314  CALL-OUTPUT.                                                     ELGGXXC 
01315      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELGGXXC 
01316                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELGGXXC 
01317      INITIALIZE TCAR-FROM-AREA.                                   ELGGXXC 
01318                                                                   ELGGXXC 
01319 ************************************************************      ELGGXXC 
01320 *                                                          *      ELGGXXC 
01321 *        EMPTY COST CONTAINMENT TAB                        *      ELGGXXC 
01322 *                                                          *      ELGGXXC 
01323 ************************************************************      ELGGXXC 
01324  EMPTY-COST-CONTAINMENT-TAB.                                      ELGGXXC 
01325      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXC 
01326      MOVE SPACES  TO  COF-DTL-LINE(COF-NBR-DTL-LINES).            ELGGXXC 
01327      MOVE SRP-TABULAR-SLOT-NO  TO  WS-TAB-SLOT-NO.                ELGGXXC 
01328      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXC 
01329      STRING 'THIS ',  SRP-CCP-NAME DELIMITED BY '  '              ELGGXXC 
01330             ' WITH TABULAR SLOT NO. ',  WS-TAB-SLOT-NO,           ELGGXXC 
01331             ' HAS AN EMPTY TABULAR RECORD'  DELIMITED BY          ELGGXXC 
01332          SIZE                                                     ELGGXXC 
01333             INTO   COF-DTL-LINE(COF-NBR-DTL-LINES).               ELGGXXC 
01334      PERFORM CALL-OUTPUT.                                         ELGGXXC 
01335                                                                   ELGGXXC 
01336 ************************************************************      ELGGXXC 
01337 *                                                          *      ELGGXXC 
01338 *        TABULAR NOT FOUND                                 *      ELGGXXC 
01339 *                                                          *      ELGGXXC 
01340 ************************************************************      ELGGXXC 
01341  TABULAR-NOT-FOUND.                                               ELGGXXC 
01342      SET CIA-AB-NOTFND-GCTABULR  TO  TRUE.                        ELGGXXC 
01343      EXEC CICS  ABEND  ABCODE(CIA-ABCODE)  END-EXEC.              ELGGXXC 
01344                                                                   ELGGXXC 
01345 ************************************************************      ELGGXXC 
01346 *                                                          *      ELGGXXC 
01347 *        PROVIDER FILE PROBLEM                             *      ELGGXXC 
01348 *                                                          *      ELGGXXC 
01349 ************************************************************      ELGGXXC 
01350  PROVIDER-FILE-PROBLEM.                                           ELGGXXC 
01351      ADD 1  TO  COF-NBR-DTL-LINES.                                ELGGXXC 
01352      STRING 'PROVIDER NUMBER ''',   PDB-I-PROVIDER-NBR            ELGGXXC 
01353             ''' IS NOT ON FILE'   DELIMITED BY SIZE               ELGGXXC 
01354          INTO                                                     ELGGXXC 
01355                                                                   ELGGXXC 
01356          COF-DTL-LINE(COF-NBR-DTL-LINES).                         ELGGXXC 
