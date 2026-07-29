00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSNOTE1
00003  PROGRAM-ID.         ELSNOTE1.                                       LV002
00004                                                                   ELSNOTE1
00005  AUTHOR.             EDWARD G LISS                                ELSNOTE1
00006                                                                   ELSNOTE1
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSNOTE1
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSNOTE1
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSNOTE1
00010                      233 N. MICHIGAN AVE                          ELSNOTE1
00011                      CHICAGO, ILLINOIS 60601                      ELSNOTE1
00012                                                                   ELSNOTE1
00013  DATE-WRITTEN.       12-JAN-1988.                                 ELSNOTE1
00014                                                                   ELSNOTE1
00015  DATE-COMPILED.                                                   ELSNOTE1
00016                                                                   ELSNOTE1
00017  SECURITY.           COPYRIGHT 1988,                              ELSNOTE1
00018                      HEALTH CARE SERVICE CORPORATION              ELSNOTE1
00019  ENVIRONMENT DIVISION.                                            ELSNOTE1
00020                                                                   ELSNOTE1
00021  CONFIGURATION SECTION.                                           ELSNOTE1
00022  SOURCE-COMPUTER.    IBM-3033.                                    ELSNOTE1
00023  OBJECT-COMPUTER.    IBM-3033.                                    ELSNOTE1
00024                                                                   ELSNOTE1
00025 ******************************************************************ELSNOTE1
00026 *                                                                *ELSNOTE1
00027 *    PROGRAM:    ELSNOTE1                                        *ELSNOTE1
00028 *    TITLE: 'ELS PPO NETWORK NOTIFICATION SCREEN                 *ELSNOTE1
00029 *    DATE:       12-JAN-1988                                     *ELSNOTE1
00030 *    AUTHOR:     EDWARD G LISS                                   *ELSNOTE1
00031 *    FUNCTION:                                                   *ELSNOTE1
00032 *      THIS MODULE IS THE KEY SELECTOR MODULE.  IT DECIDES       *ELSNOTE1
00033 *      WHICH MODULE NEEDS TO BE CALLED TO COMPLETE THE KEY       *ELSNOTE1
00034 *      INFORMATION.                                              *ELSNOTE1
00035 *                                                                *ELSNOTE1
00036 *    NOTES:                                                      *ELSNOTE1
00037 *                                                                *ELSNOTE1
00038 ******************************************************************ELSNOTE1
00039 *                                                                *ELSNOTE1
00040 *                      MAINTENANCE HISTORY                       *ELSNOTE1
00041 *                                                                *ELSNOTE1
00042 *  MOD     DATE     BY  DRPT                ACTION               *ELSNOTE1
00043 * ----- ----------- --- ----- ---------------------------------- *ELSNOTE1
00044 * 01.00 12-JAN-1988 EGL       CREATED                            *ELSNOTE1
00045 *                                                                *ELSNOTE1
00046 * 01.01 06-APR-1988 AKK       CHANGED PARTICIPATING TO PREFERRED *ELSNOTE1
00047 *                             PER DISCREPANCY P4727.             *ELSNOTE1
00048 *                                                                *ELSNOTE1
00049 * 01.02 20-JUN-1988 EGL       CHANGE TO IMPLEMENT NEW STORAGE    *ELSNOTE1
00050 *                             MANAGEMENT SCHEME.                 *ELSNOTE1
00051 *                                                                *ELSNOTE1
00052 * 01.03 25-AUG-1989 GEM       DESTRUCTED BY G E M                *ELSNOTE1
00053 *                                                                *ELSNOTE1
00054 * 01.04 16-NOV-1990 JPB       CHANGED REFERENCES TO KTG-         *ELSNOTE1
00055 *                             PARTICIPAT-PROV-OPTION AND WS-PPO- *ELSNOTE1
00056 *                             KEY-OPTION TO REFLECT NEW FIELD    *ELSNOTE1
00057 *                             SIZE.                              *ELSNOTE1
00058 *                                                                *ELSNOTE1
00059 * 01.05 05-MAY-1992 BAK       ADD SUPPORT FOR MCN AND POS        *ELSNOTE1
00060 *                             NOTIFICATION.                      *ELSNOTE1
00061 *                             ALSO RESTUCTURED PROGRAM TO        *ELSNOTE1
00062 *                             ELIMINATE UNNECESSARY PERFORMS.    *ELSNOTE1
00063 *                                                                *ELSNOTE1
00064 * 01.06 19-JAN-1993 AKK       ADD SUPPORT FOR RPO NOTIFICATION   *ELSNOTE1
00065 *                                                                *ELSNOTE1
00066 * 01.07 08-MAR-1993 AKK       CHANGED OUTPUT PER AUGGIE REQUEST  *ELSNOTE1
00067 *                                                                *ELSNOTE1
00068 * 01.08 03-FEB-1994 AKK       ADD SUPPORT FOR PRODUCT TYPE       *ELSNOTE1
00069 *                             DISPLAY.                           *ELSNOTE1
00070 *                                                                *ELSNOTE1
00071 * 01.09 15-APR-1994 AKK       ADD NEW NON-STANDARD PPO VALUES    *ELSNOTE1
00072 *                                                                *ELSNOTE1
00073 * 01.10 14-SEP-1994 AKK       ADD NEW NON-STANDARD PPO VALUES-0V.*ELSNOTE1
00074 *                                                                *ELSNOTE1
00075 * 01.11 20-FEB-1995 AKK       CPO HANDLING ADDED.                *ELSNOTE1
00076 *                                                                *ELSNOTE1
00077 * 01.12 23-MAY-1995 AKK       BLUE SCRIPT HANDLING.              *ELSNOTE1
00078 *                                                                *ELSNOTE1
00079 * 01.13 13-SEP-1995 AKK       ALLIANCE PRODUCT HANDLING.         *ELSNOTE1
00080 *                                                                *ELSNOTE1
00081 * 01.14 15-FEB-1996 AKK       CORRECTED ERROR THAT OCCURRED      *ELSNOTE1
00082 *                             BECAUSE BLUE INFO WAS ASKED FOR    *ELSNOTE1
00083 *                             WHEN PRODUCT TYPE WAS NEEDED.      *ELSNOTE1
00084 *                                                                *ELSNOTE1
00085 * 01.15 19-FEB-1996 AKK       ADDED SUPPORT FOR COMMUNITY BLUE   *ELSNOTE1
00086 *                             AND PREFERRED ANCILLARY CHARGES.   *ELSNOTE1
00087 *                                                                *ELSNOTE1
00088 * 01.16 10-OCT-1997 AKK       ADDED SUPPORT YR 2000 AND TEXAS    *ELSNOTE1
00089 *                             MERGE.                             *ELSNOTE1
00090 *                                                                *ELSNOTE1
00091 *       26-JAN-2000 JP        ADDED MLDATE ROUTINE TO EXPAND     *ELSNOTE1
00092 *                             WS-TO-DATE TO MM/DD/CCYY FORMAT.   *ELSNOTE1
00093 *                                                                *ELSNOTE1
00094 *       13-APR-2003 AKK       REGEN TO TEST ORDER OF COMPILES    *ELSNOTE1
00095 ******************************************************************ELSNOTE1
00096      EJECT                                                        ELSNOTE1
00097  DATA DIVISION.                                                   ELSNOTE1
00098                                                                   ELSNOTE1
00099  WORKING-STORAGE SECTION.                                         ELSNOTE1
00100 *                                                                 ELSNOTE1
00101 *                                                                 ELSNOTE1
00102  01  WS-MISC-STUFF.                                               ELSNOTE1
00103      05  ELUSTGMG-ID                    PICTURE X(8)              ELSNOTE1
00104                                                 VALUE 'ELUSTGMG'. ELSNOTE1
00105      05  WS-DUMMY-PTR                   POINTER.                  ELSNOTE1
00106      05  WS-HOLD-ALLNC-TYPE.                                      ELSNOTE1
00107          10  WS-FIRST-CHRCTR            PICTURE X(1).             ELSNOTE1
00108          10  WS-RMNG-CHRCTR             PICTURE X(8).             ELSNOTE1
00109      05  WS-MENU-2-INITIALIZING-SW      PIC X     VALUE 'N'.      ELSNOTE1
00110          88  MENU-2-INITIALIZED                   VALUE '2'.      ELSNOTE1
00111          88  MENU-2-NOT-INITIALIZED               VALUE 'N'.      ELSNOTE1
00112      05  WS-ALLIANCE-NOT-DONE-SW        PIC X     VALUE 'N'.      ELSNOTE1
00113          88  WS-ALLIANCE-NOT-DONE                 VALUE 'N'.      ELSNOTE1
00114          88  WS-ALLIANCE-DONE                     VALUE 'Y'.      ELSNOTE1
00115      05  WS-PROCESS-SWITCH          PIC X     VALUE SPACE.        ELSNOTE1
00116          88  PROCESSING-RPO                       VALUE 'R'.      ELSNOTE1
00117          88  PROCESSING-CPO                       VALUE 'C'.      ELSNOTE1
00118          88  PROCESSING-PPO                       VALUE 'P'.      ELSNOTE1
00119          88  PROCESSING-POS                       VALUE 'S'.      ELSNOTE1
00120          88  PROCESSING-MCN                       VALUE 'M'.      ELSNOTE1
00121          88  PROCESSING-PRD                       VALUE 'T'.      ELSNOTE1
00122          88  PROCESSING-BLS                       VALUE 'B'.      ELSNOTE1
00123          88  PROCESSING-ALL                       VALUE 'A'.      ELSNOTE1
00124          88  PROCESSING-CBL                       VALUE 'O'.      ELSNOTE1
00125          88  PROCESSING-PAN                       VALUE 'N'.      ELSNOTE1
00126                                                                   ELSNOTE1
00127      05  WS-DUPLICATE-SW                PICTURE X VALUE 'N'.      ELSNOTE1
00128          88  WS-DUPLICATE                         VALUE 'Y'.      ELSNOTE1
00129          88  WS-NO-DUPLICATE                      VALUE 'N'.      ELSNOTE1
00130      05  WS-DISPLAY-SW                  PICTURE X VALUE 'Y'.      ELSNOTE1
00131          88  WS-FIRST-DISPLAY                     VALUE 'Y'.      ELSNOTE1
00132          88  WS-NOT-FIRST-DISPLAY                 VALUE 'N'.      ELSNOTE1
00133      05  WS-DATE-DISPLAY-SW             PICTURE X VALUE 'N'.      ELSNOTE1
00134          88  WS-DATES-DISPLAYED                   VALUE 'Y'.      ELSNOTE1
00135          88  WS-NO-DATES-DISPLAYED                VALUE 'N'.      ELSNOTE1
00136      05  WS-BEN-NRTV-DISPLAYED          PICTURE X VALUE 'N'.      ELSNOTE1
00137          88  WS-BEN-DISPLAYED                     VALUE 'Y'.      ELSNOTE1
00138          88  WS-BEN-NOT-DISPLAYED                 VALUE 'N'.      ELSNOTE1
00139      05  WS-HOLD-IDX                    USAGE INDEX.              ELSNOTE1
00140      05  WS-HOLD-EFF-DT                 PICTURE S9(7) COMP-3      ELSNOTE1
00141                                                   VALUE ZEROES.   ELSNOTE1
00142      05  WS-HOLD-TRMN-DT                PICTURE S9(7) COMP-3      ELSNOTE1
00143                                                   VALUE ZEROES.   ELSNOTE1
00144      05  WS-MOVE-SUB                    PICTURE S9(4) COMP.       ELSNOTE1
00145      05  WS-GROUP-TALLY                 PICTURE S9(4) COMP        ELSNOTE1
00146                                                   VALUE ZERO.     ELSNOTE1
00147      05  WS-GRP-CNT                     PICTURE S9 VALUE ZERO.    ELSNOTE1
00148      05  WS-INIT                        PICTURE S9 VALUE ZERO.    ELSNOTE1
00149      05  WS-DATE.                                                 ELSNOTE1
00150          10  FILLER                     PICTURE X(6)              ELSNOTE1
00151                                            VALUE 'FROM'.          ELSNOTE1
00152          10  WS-FROM-DATE               PICTURE 99/99/99.         ELSNOTE1
00153          10  FILLER                     PICTURE X(02).            ELSNOTE1
00154          10  FILLER                     PICTURE X(6) VALUE 'TO'.  ELSNOTE1
00155          10  WS-TO-DATE                 PICTURE 99/99/9999.       ELSNOTE1
00156                                                                   ELSNOTE1
00157      05  WS-YEAR-END-DATE.                                        ELSNOTE1
00158          10  WS-MONTH                   PICTURE 9(02) VALUE 12.   ELSNOTE1
00159          10 FILLER                      PICTURE X VALUE '/'.      ELSNOTE1
00160          10  WS-DAY                     PICTURE 9(02) VALUE 31.   ELSNOTE1
00161          10 FILLER                      PICTURE X VALUE '/'.      ELSNOTE1
00162          10  WS-YEAR                    PICTURE 9(04) VALUE 9999. ELSNOTE1
00163                                                                   ELSNOTE1
00164      05  WS-TEMP-KEY.                                             ELSNOTE1
00165          10  WS-TEMP-KEY-OPTION          PICTURE XX.              ELSNOTE1
00166          10  WS-TEMP-KEY-EFF-DT          PICTURE S9(7) COMP-3.    ELSNOTE1
00167          10  WS-TEMP-KEY-TERMN-DT        PICTURE S9(7) COMP-3.    ELSNOTE1
00168      05  WS-PPO-KEY.                                              ELSNOTE1
00169          10  WS-CPO-KEY-OPTION          PICTURE XX.               ELSNOTE1
00170          10  WS-CPO-KEY-EFF-DT          PICTURE S9(7) COMP-3.     ELSNOTE1
00171          10  WS-CPO-KEY-TERMN-DT        PICTURE S9(7) COMP-3.     ELSNOTE1
00172      05  WS-PPO-KEY.                                              ELSNOTE1
00173          10  WS-PPO-KEY-OPTION          PICTURE XX.               ELSNOTE1
00174          10  WS-PPO-KEY-EFF-DT          PICTURE S9(7) COMP-3.     ELSNOTE1
00175          10  WS-PPO-KEY-TERMN-DT        PICTURE S9(7) COMP-3.     ELSNOTE1
00176      05  WS-MCN-KEY.                                              ELSNOTE1
00177          10  WS-MCN-KEY-OPTION          PICTURE XX.               ELSNOTE1
00178          10  WS-MCN-KEY-EFF-DT          PICTURE S9(7) COMP-3.     ELSNOTE1
00179          10  WS-MCN-KEY-TERMN-DT        PICTURE S9(7) COMP-3.     ELSNOTE1
00180      05  WS-POS-KEY.                                              ELSNOTE1
00181          10  WS-POS-KEY-OPTION          PICTURE XX.               ELSNOTE1
00182          10  WS-POS-KEY-EFF-DT          PICTURE S9(7) COMP-3.     ELSNOTE1
00183          10  WS-POS-KEY-TERMN-DT        PICTURE S9(7) COMP-3.     ELSNOTE1
00184      05  WS-RPO-KEY.                                              ELSNOTE1
00185          10  WS-RPO-KEY-OPTION          PICTURE XX.               ELSNOTE1
00186          10  WS-RPO-KEY-EFF-DT          PICTURE S9(7) COMP-3.     ELSNOTE1
00187          10  WS-RPO-KEY-TERMN-DT        PICTURE S9(7) COMP-3.     ELSNOTE1
00188      05  WS-CBL-KEY.                                              ELSNOTE1
00189          10  WS-CBL-KEY-OPTION          PICTURE XX.               ELSNOTE1
00190          10  WS-CBL-KEY-EFF-DT          PICTURE S9(7) COMP-3.     ELSNOTE1
00191          10  WS-CBL-KEY-TERMN-DT        PICTURE S9(7) COMP-3.     ELSNOTE1
00192      05  WS-PAN-KEY.                                              ELSNOTE1
00193          10  WS-PAN-KEY-OPTION          PICTURE XX.               ELSNOTE1
00194          10  WS-PAN-KEY-EFF-DT          PICTURE S9(7) COMP-3.     ELSNOTE1
00195          10  WS-PAN-KEY-TERMN-DT        PICTURE S9(7) COMP-3.     ELSNOTE1
00196      05  WS-DETAIL.                                               ELSNOTE1
00197          10  WS-FIRST-LINE              PICTURE X(79).            ELSNOTE1
00198          10  FILLER    REDEFINES WS-FIRST-LINE.                   ELSNOTE1
00199              15  WS-BLANKS              PICTURE X(9).             ELSNOTE1
00200              15  WS-NEXT-LINE           PICTURE X(70).            ELSNOTE1
00201                                                                   ELSNOTE1
00202 ****WS-DETAIL-LINE-1 INCLUDES THE DATE INFO - ALL LINES NOT   *   ELSNOTE1
00203 ****INCLUDING A DATE WILL BE DISPLAYED FROM WS-DETAIL-LINE-2  *   ELSNOTE1
00204                                                                   ELSNOTE1
00205      05  WS-DETAIL-LINE-1               PICTURE X(79).            ELSNOTE1
00206      05  WS-DETAIL-LINE-ENTRY                                     ELSNOTE1
00207            OCCURS 3 TIMES INDEXED BY WS-IDX.                      ELSNOTE1
00208            10 WS-DETAIL-LINE-2.                                   ELSNOTE1
00209              15  WS-SPACES              PICTURE X(28).            ELSNOTE1
00210              15  WS-DETAIL-LINE-2A      PICTURE X(51).            ELSNOTE1
00211                                                                   ELSNOTE1
00212      05  WS-RPO-DETAIL.                                           ELSNOTE1
00213          10  WS-FIRST-RPO-LINE          PICTURE X(79).            ELSNOTE1
00214          10  FILLER    REDEFINES WS-FIRST-RPO-LINE.               ELSNOTE1
00215              15  WS-BLANKS-R            PICTURE X(9).             ELSNOTE1
00216              15  WS-NEXT-RPO-LINE       PICTURE X(70).            ELSNOTE1
00217                                                                   ELSNOTE1
00218      05  WS-PPO-DETAIL.                                           ELSNOTE1
00219          10  WS-FIRST-PPO-LINE          PICTURE X(79).            ELSNOTE1
00220          10  FILLER    REDEFINES WS-FIRST-PPO-LINE.               ELSNOTE1
00221              15  WS-BLANKS-P            PICTURE X(9).             ELSNOTE1
00222              15  WS-NEXT-PPO-LINE       PICTURE X(70).            ELSNOTE1
00223                                                                   ELSNOTE1
00224      05  WS-NUM-HEADING-LINES       PICTURE S9(4) COMP VALUE +2.  ELSNOTE1
00225      05  WS-SCREEN-TITLES.                                        ELSNOTE1
00226          10  WS-SCREEN-TITLE-1.                                   ELSNOTE1
00227              15  FILLER       PIC X(79) VALUE                     ELSNOTE1
00228              'THE FOLLOWING SPECIAL PROGRAM(S) APPLY TO THIS CONTRELSNOTE1
00229 -            'ACT:'.                                              ELSNOTE1
00230                                                                   ELSNOTE1
00231      05  WS-NON-STANDARD-VALUES     PIC X(02).                    ELSNOTE1
00232          88 WS-NON-STANDARD                VALUE                  ELSNOTE1
00233            '0A' '0B' '0C' '0D' '0E' '0F' '0H' '0I'                ELSNOTE1
00234            '0M' '0N' '0P' '0Q' '0R' '0S' '0T' '0U' '0V' '08'      ELSNOTE1
00235            '09' '10'.                                             ELSNOTE1
00236                                                                   ELSNOTE1
00237      05  WS-NON-STANDARD-RPO-VALUES PIC X(02).                    ELSNOTE1
00238          88 WS-NON-STANDARD-RPO            VALUE                  ELSNOTE1
00239            '0A' '0B' '0C' '0D' '0E' '0F' '0H'                     ELSNOTE1
00240            '0M' '0N' '0P' '02' '03' '08' '09'.                    ELSNOTE1
00241                                                                   ELSNOTE1
00242      05  WS-NON-STANDARD-CPO-VALUES PIC X(02).                    ELSNOTE1
00243          88 WS-NON-STANDARD-CPO            VALUE                  ELSNOTE1
00244            '01'.                                                  ELSNOTE1
00245                                                                   ELSNOTE1
00246      05  WS-NON-STANDARD-CBL-VALUES PIC X(02).                    ELSNOTE1
00247          88 WS-NON-STANDARD-CBL            VALUE                  ELSNOTE1
00248            '  '.                                                  ELSNOTE1
00249                                                                   ELSNOTE1
00250      05  WS-NON-STANDARD-PAN-VALUES PIC X(02).                    ELSNOTE1
00251          88 WS-NON-STANDARD-PAN            VALUE                  ELSNOTE1
00252            '  '.                                                  ELSNOTE1
00253                                                                   ELSNOTE1
00254      05  WS-TPPO  PIC X(29) VALUE ' *PREFERRED PROVIDER OPTION: '.ELSNOTE1
00255      05  WS-TCPO  PIC X(34) VALUE                                 ELSNOTE1
00256                   ' *COMMUNITY PARTICIPATING OPTION: '.           ELSNOTE1
00257      05  WS-TRPO  PIC X(30) VALUE                                 ELSNOTE1
00258                            ' *RESTRICTED PROVIDER OPTION: '.      ELSNOTE1
00259      05  WS-TMCN  PIC X(24) VALUE ' *MANAGED CARE NETWORK: '.     ELSNOTE1
00260      05  WS-TPOS  PIC X(20) VALUE ' *POINT OF SERVICE: '.         ELSNOTE1
00261      05  WS-TCBL  PIC X(28) VALUE ' *COMMUNITY BLUE PROGRAM:  '.  ELSNOTE1
00262      05  WS-TPAN  PIC X(33)                                       ELSNOTE1
00263                   VALUE ' *PREFERRED ANCILLARY NETWORK:  '.       ELSNOTE1
00264                                                                   ELSNOTE1
00265      05  WS-DEFAULT-RESPONSE        PICTURE X(25) VALUE           ELSNOTE1
00266              'PRESS <ENTER> TO CONTINUE'.                         ELSNOTE1
00267      05  WS-CPO-TOPIC-REF-1         PICTURE X(24) VALUE           ELSNOTE1
00268              '. SEE CPO NETWORK TOPIC '.                          ELSNOTE1
00269      05  WS-CPO-TOPIC-REF-2         PICTURE X(62) VALUE           ELSNOTE1
00270       'FOR ADDITIONAL INFORMATION.'.                              ELSNOTE1
00271      05  WS-RPO-TOPIC-REF-1         PICTURE X(24) VALUE           ELSNOTE1
00272              '. SEE RPO NETWORK TOPIC '.                          ELSNOTE1
00273      05  WS-RPO-TOPIC-REF-2         PICTURE X(62) VALUE           ELSNOTE1
00274       'FOR ADDITIONAL INFORMATION.'.                              ELSNOTE1
00275      05  WS-CBL-TOPIC-REF-1         PICTURE X(24) VALUE           ELSNOTE1
00276              '. SEE CBL NETWORK TOPIC '.                          ELSNOTE1
00277      05  WS-CBL-TOPIC-REF-2         PICTURE X(62) VALUE           ELSNOTE1
00278       'FOR ADDITIONAL INFORMATION.'.                              ELSNOTE1
00279      05  WS-PAN-TOPIC-REF-1         PICTURE X(24) VALUE           ELSNOTE1
00280              '. SEE PAN NETWORK TOPIC '.                          ELSNOTE1
00281      05  WS-PAN-TOPIC-REF-2         PICTURE X(62) VALUE           ELSNOTE1
00282       'FOR ADDITIONAL INFORMATION.'.                              ELSNOTE1
00283      05  WS-PPO-TOPIC-REF-1         PICTURE X(24) VALUE           ELSNOTE1
00284              '. SEE PPO NETWORK TOPIC.'.                          ELSNOTE1
00285      05  WS-PPO-MENU-TITLE     PICTURE X(40) VALUE                ELSNOTE1
00286          '  ****SPECIAL OPTION NOTIFICATION****   '.              ELSNOTE1
00287      05  WS-BNFT-PRVSN-TPC1    PICTURE X(48) VALUE                ELSNOTE1
00288          ' *SPECIAL BENEFIT RULES APPLY TO THIS CONTRACT. '.      ELSNOTE1
00289      05  WS-BNFT-PRVSN-TPC2    PICTURE X(57) VALUE                ELSNOTE1
00290         'SEE BENEFIT EXCEPTION NARRATIVE TOPIC (BEN) FOR DETAILS'.ELSNOTE1
00291      05  WS-BLUE-SCRIPT-TPC1   PICTURE X(53) VALUE                ELSNOTE1
00292          '*PRESCRIPTION DRUGS WILL BE ELECTRONICALLY SUBMITTED'.  ELSNOTE1
00293      05  WS-BLUE-SCRIPT-TPC2   PICTURE X(51) VALUE                ELSNOTE1
00294          'BY A PARTICIPATING PHARMACY FOR THIS GROUP/SECTION.'.   ELSNOTE1
00295      05  WS-ALLIANCE-TOPIC     PICTURE X(26) VALUE                ELSNOTE1
00296          'THIS IS AN ALLIANCE GROUP.'.                            ELSNOTE1
00297      EJECT                                                        ELSNOTE1
00298  01  HGADATE-PARM.  COPY HGCDAT01.                                ELSNOTE1
00299      EJECT                                                        ELSNOTE1
00300  COPY MLDATE01.                                                   ELSNOTE1
00301      EJECT                                                        ELSNOTE1
00302  COPY ELSTCWAC.                                                   ELSNOTE1
00303      EJECT                                                        ELSNOTE1
00304  LINKAGE SECTION.                                                 ELSNOTE1
00305      EJECT                                                        ELSNOTE1
00306  01  DFHCOMMAREA.                                                 ELSNOTE1
00307  COPY ELSCOMMC.                                                   ELSNOTE1
00308      EJECT                                                        ELSNOTE1
00309  COPY ELSCIA2C.                                                   ELSNOTE1
00310                                                                   ELSNOTE1
00311  COPY ELSSSCBC.                                                   ELSNOTE1
00312      EJECT                                                        ELSNOTE1
00313  COPY ELSKTBGC.                                                   ELSNOTE1
00314                                                                   ELSNOTE1
00315  COPY ELSMHDGC.                                                   ELSNOTE1
00316      EJECT                                                        ELSNOTE1
00317  COPY ELSMOPTC.                                                   ELSNOTE1
00318                                                                   ELSNOTE1
00319  COPY ELSMENUC.                                                   ELSNOTE1
00320      EJECT                                                        ELSNOTE1
00321  COPY ELSIOPMC.                                                   ELSNOTE1
00322      EJECT                                                        ELSNOTE1
00323  COPY ELSCMIFC.                                                   ELSNOTE1
00324                                                                   ELSNOTE1
00325  COPY ELSCMDSC.                                                   ELSNOTE1
00326      EJECT                                                        ELSNOTE1
00327  PROCEDURE DIVISION.                                              ELSNOTE1
00328 ************************************************************      ELSNOTE1
00329 *                                                          *      ELSNOTE1
00330 *        SPECIAL NOTIFICATION SCREENS                      *      ELSNOTE1
00331 *                                                          *      ELSNOTE1
00332 ************************************************************      ELSNOTE1
00333  SPECIAL-NOTIFICATION-SCREENS.                                    ELSNOTE1
00334      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELSNOTE1
00335          SET CIA-AB-DFHCOMMAREA TO TRUE                           ELSNOTE1
00336          EXEC CICS ABEND                                          ELSNOTE1
00337                ABCODE(CIA-ABCODE)                                 ELSNOTE1
00338                END-EXEC.                                          ELSNOTE1
00339      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSNOTE1
00340          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSNOTE1
00341      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSNOTE1
00342      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSNOTE1
00343                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELSNOTE1
00344                                                                   ELSNOTE1
00345      PERFORM VARYING WS-IDX FROM 1 BY 1                           ELSNOTE1
00346        UNTIL WS-IDX > 3                                           ELSNOTE1
00347           INITIALIZE WS-SPACES (WS-IDX)                           ELSNOTE1
00348           INITIALIZE WS-DETAIL-LINE-2A (WS-IDX)                   ELSNOTE1
00349      END-PERFORM.                                                 ELSNOTE1
00350      PERFORM MAIN-PROCESSING.                                     ELSNOTE1
00351      GOBACK.                                                      ELSNOTE1
00352                                                                   ELSNOTE1
00353 ************************************************************      ELSNOTE1
00354 *                                                          *      ELSNOTE1
00355 *        MAIN PROCESSING                                   *      ELSNOTE1
00356 *                                                          *      ELSNOTE1
00357 ************************************************************      ELSNOTE1
00358  MAIN-PROCESSING.                                                 ELSNOTE1
00359      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSNOTE1
00360          PERFORM PREPARE-LIST-MENU                                ELSNOTE1
00361      ELSE IF SSB-MENU-COMPLETE (SSB-SELECTOR-STATE)               ELSNOTE1
00362          SET SSB-NOT-USED (SSB-SELECTOR-STATE) TO TRUE            ELSNOTE1
00363      ELSE                                                         ELSNOTE1
00364          SET CIA-AB-UNDEF TO TRUE                                 ELSNOTE1
00365          EXEC CICS ABEND                                          ELSNOTE1
00366                ABCODE(CIA-ABCODE)                                 ELSNOTE1
00367                END-EXEC.                                          ELSNOTE1
00368                                                                   ELSNOTE1
00369      EJECT                                                        ELSNOTE1
00370 ************************************************************      ELSNOTE1
00371 *                                                          *      ELSNOTE1
00372 *        PREPARE LIST MENU                                 *      ELSNOTE1
00373 *                                                          *      ELSNOTE1
00374 ************************************************************      ELSNOTE1
00375  PREPARE-LIST-MENU.                                               ELSNOTE1
00376      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSNOTE1
00377      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSNOTE1
00378          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSNOTE1
00379                                                                   ELSNOTE1
00380      IF CIA-RC-PTR-NULL                                           ELSNOTE1
00381          SET CIA-ELSCMIF-DDN   TO  TRUE                           ELSNOTE1
00382          SET CIA-STG-GETMAIN   TO  TRUE                           ELSNOTE1
00383          EXEC CICS LINK PROGRAM('ELUSTGMG')                       ELSNOTE1
00384                     COMMAREA(DFHCOMMAREA)                         ELSNOTE1
00385                     END-EXEC.                                     ELSNOTE1
00386                                                                   ELSNOTE1
00387          SET CIA-ELSCMIF-DDN TO TRUE                              ELSNOTE1
00388          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSNOTE1
00389          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSNOTE1
00390                                                                   ELSNOTE1
00391      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELSNOTE1
00392      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSNOTE1
00393          ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                       ELSNOTE1
00394                                                                   ELSNOTE1
00395      IF CIA-RC-PTR-NULL                                           ELSNOTE1
00396          SET CIA-ELSKTBG-DDN   TO  TRUE                           ELSNOTE1
00397          SET CIA-STG-RETRIEVE  TO  TRUE                           ELSNOTE1
00398          EXEC CICS LINK PROGRAM('ELUSTGMG')                       ELSNOTE1
00399                     COMMAREA(DFHCOMMAREA)                         ELSNOTE1
00400                     END-EXEC                                      ELSNOTE1
00401          SET CIA-ELSKTBG-DDN TO TRUE                              ELSNOTE1
00402          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSNOTE1
00403             ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                    ELSNOTE1
00404                                                                   ELSNOTE1
00405      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSNOTE1
00406      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSNOTE1
00407          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSNOTE1
00408                                                                   ELSNOTE1
00409      IF CIA-RC-PTR-NULL                                           ELSNOTE1
00410          SET CIA-ELSMENU-DDN    TO  TRUE                          ELSNOTE1
00411          SET CIA-STG-GETMAIN    TO  TRUE                          ELSNOTE1
00412          EXEC CICS LINK PROGRAM('ELUSTGMG')                       ELSNOTE1
00413                     COMMAREA(DFHCOMMAREA)                         ELSNOTE1
00414                     END-EXEC                                      ELSNOTE1
00415          SET CIA-ELSMENU-DDN TO TRUE                              ELSNOTE1
00416          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELSNOTE1
00417              ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.              ELSNOTE1
00418                                                                   ELSNOTE1
00419      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSNOTE1
00420      SET IOP-DEL TO TRUE.                                         ELSNOTE1
00421      SET IOP-FCQ-NONE TO TRUE.                                    ELSNOTE1
00422      SET IOP-KVQ-NONE TO TRUE.                                    ELSNOTE1
00423      EXEC CICS LINK                                               ELSNOTE1
00424                PROGRAM('ELUIOPGM')                                ELSNOTE1
00425                COMMAREA(DFHCOMMAREA)                              ELSNOTE1
00426                END-EXEC.                                          ELSNOTE1
00427                                                                   ELSNOTE1
00428      MOVE ZEROES TO WS-CPO-KEY-OPTION.                            ELSNOTE1
00429      MOVE ZEROES TO WS-MCN-KEY-OPTION.                            ELSNOTE1
00430      MOVE ZEROES TO WS-POS-KEY-OPTION.                            ELSNOTE1
00431      MOVE ZEROES TO WS-PPO-KEY-OPTION.                            ELSNOTE1
00432      MOVE ZEROES TO WS-RPO-KEY-OPTION.                            ELSNOTE1
00433      MOVE ZEROES TO WS-CBL-KEY-OPTION.                            ELSNOTE1
00434      MOVE ZEROES TO WS-PAN-KEY-OPTION.                            ELSNOTE1
00435                                                                   ELSNOTE1
00436      PERFORM INITIALIZE-MENU-1.                                   ELSNOTE1
00437                                                                   ELSNOTE1
00438      PERFORM DETERMINE-IF-GROUP-APPLIES                           ELSNOTE1
00439          VARYING KTG-IDX FROM 1 BY 1                              ELSNOTE1
00440                  UNTIL KTG-IDX > KTG-NBR-KEYS.                    ELSNOTE1
00441                                                                   ELSNOTE1
00442      SET SSB-START-MENU (SSB-SELECTOR-STATE)                      ELSNOTE1
00443          TO TRUE.                                                 ELSNOTE1
00444                                                                   ELSNOTE1
00445      EJECT                                                        ELSNOTE1
00446                                                                   ELSNOTE1
00447 ************************************************************      ELSNOTE1
00448 *                                                          *      ELSNOTE1
00449 *        INITIALIZE MENU-1                                 *      ELSNOTE1
00450 *                                                          *      ELSNOTE1
00451 ************************************************************      ELSNOTE1
00452  INITIALIZE-MENU-1.                                               ELSNOTE1
00453      PERFORM TALLY-GROUPS-AVAILABLE.                              ELSNOTE1
00454      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSNOTE1
00455      COMPUTE                                                      ELSNOTE1
00456         CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +           ELSNOTE1
00457                    LENGTH OF MSO-MENU-OPT *                       ELSNOTE1
00458                         WS-GROUP-TALLY.                           ELSNOTE1
00459      SET CIA-STG-GETMAIN TO TRUE.                                 ELSNOTE1
00460      EXEC CICS LINK PROGRAM('ELUSTGMG')                           ELSNOTE1
00461                COMMAREA(DFHCOMMAREA)                              ELSNOTE1
00462                 END-EXEC.                                         ELSNOTE1
00463                                                                   ELSNOTE1
00464      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSNOTE1
00465      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSNOTE1
00466          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSNOTE1
00467      MOVE 2 TO MSO-OPT-LEN.                                       ELSNOTE1
00468      SET MSO-OPT-TYP-AN TO TRUE.                                  ELSNOTE1
00469      MOVE ZERO TO MSO-NBR-MENU-OPTS.                              ELSNOTE1
00470      MOVE WS-DEFAULT-RESPONSE TO SSB-MNU-CHOICE-TABLE.            ELSNOTE1
00471      MOVE ZERO    TO MSO-MIN-CHOICES                              ELSNOTE1
00472                      MSO-MAX-CHOICES.                             ELSNOTE1
00473                                                                   ELSNOTE1
00474      COMPUTE                                                      ELSNOTE1
00475         CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +              ELSNOTE1
00476                    LENGTH OF MHD-HDG-LINE *                       ELSNOTE1
00477                        WS-NUM-HEADING-LINES.                      ELSNOTE1
00478      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSNOTE1
00479      SET CIA-STG-GETMAIN TO TRUE.                                 ELSNOTE1
00480      EXEC CICS LINK PROGRAM('ELUSTGMG')                           ELSNOTE1
00481                COMMAREA(DFHCOMMAREA)                              ELSNOTE1
00482                 END-EXEC.                                         ELSNOTE1
00483                                                                   ELSNOTE1
00484      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSNOTE1
00485      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSNOTE1
00486          ADDRESS OF MHD-MENU-HEADINGS.                            ELSNOTE1
00487 ************************************************************      ELSNOTE1
00488 *                                                          *      ELSNOTE1
00489 *        TALLY GROUPS AVAILABLE                            *      ELSNOTE1
00490 *                                                          *      ELSNOTE1
00491 ************************************************************      ELSNOTE1
00492  TALLY-GROUPS-AVAILABLE.                                          ELSNOTE1
00493      PERFORM COUNT-GROUPS                                         ELSNOTE1
00494          VARYING KTG-IDX FROM 1 BY 1                              ELSNOTE1
00495                     UNTIL KTG-IDX > KTG-NBR-KEYS.                 ELSNOTE1
00496                                                                   ELSNOTE1
00497 ************************************************************      ELSNOTE1
00498 *                                                          *      ELSNOTE1
00499 *        COUNT GROUPS                                      *      ELSNOTE1
00500 *                                                          *      ELSNOTE1
00501 ************************************************************      ELSNOTE1
00502  COUNT-GROUPS.                                                    ELSNOTE1
00503      IF KTG-PARTICIPAT-PROV-OPTION (KTG-IDX) NOT = ZERO           ELSNOTE1
00504         ADD 1 TO WS-GROUP-TALLY.                                  ELSNOTE1
00505      IF KTG-CBL-PARTICP-IND (KTG-IDX) NOT = ZERO                  ELSNOTE1
00506         ADD 1 TO WS-GROUP-TALLY.                                  ELSNOTE1
00507      IF KTG-CPO-PARTICP-IND (KTG-IDX) NOT = ZERO                  ELSNOTE1
00508         ADD 1 TO WS-GROUP-TALLY.                                  ELSNOTE1
00509      IF KTG-RPO-PARTICP-IND (KTG-IDX) NOT = ZERO                  ELSNOTE1
00510         ADD 1 TO WS-GROUP-TALLY.                                  ELSNOTE1
00511      IF KTG-PAN-PARTICP-IND (KTG-IDX) NOT = ZERO                  ELSNOTE1
00512         ADD 1 TO WS-GROUP-TALLY.                                  ELSNOTE1
00513      IF KTG-POS-PARTICP-IND (KTG-IDX) NOT = ZERO                  ELSNOTE1
00514         ADD 1 TO WS-GROUP-TALLY.                                  ELSNOTE1
00515      IF KTG-NEW-POS-IND (KTG-IDX) NOT = ZERO                      ELSNOTE1
00516         ADD 1 TO WS-GROUP-TALLY.                                  ELSNOTE1
00517      IF KTG-PRODUCT-TYPE (KTG-IDX) NOT = ZERO                     ELSNOTE1
00518         ADD 1 TO WS-GROUP-TALLY.                                  ELSNOTE1
00519      IF KTG-BLUE-SCRIPT (KTG-IDX) NOT = ZERO                      ELSNOTE1
00520         ADD 1 TO WS-GROUP-TALLY.                                  ELSNOTE1
00521      IF WS-ALLIANCE-NOT-DONE                                      ELSNOTE1
00522         IF KTG-ALLIANCE-IND NOT = ZERO                            ELSNOTE1
00523            ADD 1 TO WS-GROUP-TALLY                                ELSNOTE1
00524 *          SET WS-ALLIANCE-DONE TO TRUE                           ELSNOTE1
00525      END-IF.                                                      ELSNOTE1
00526                                                                   ELSNOTE1
00527 ************************************************************      ELSNOTE1
00528 *                                                          *      ELSNOTE1
00529 *        DETERMINE IF GROUP APPLIES                        *      ELSNOTE1
00530 *                                                          *      ELSNOTE1
00531 ************************************************************      ELSNOTE1
00532  DETERMINE-IF-GROUP-APPLIES.                                      ELSNOTE1
00533      MOVE ZERO TO WS-GRP-CNT.                                     ELSNOTE1
00534      SET WS-NO-DATES-DISPLAYED TO TRUE.                           ELSNOTE1
00535      IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00536         KTG-RPO-PARTICP-IND (KTG-IDX) NOT = ZEROS                 ELSNOTE1
00537         PERFORM BUILD-AND-PROCESS-RPO.                            ELSNOTE1
00538      IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00539         KTG-CPO-PARTICP-IND (KTG-IDX) NOT = ZEROS                 ELSNOTE1
00540         PERFORM BUILD-AND-PROCESS-CPO.                            ELSNOTE1
00541      IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00542         KTG-CBL-PARTICP-IND (KTG-IDX) NOT = ZEROS                 ELSNOTE1
00543         PERFORM BUILD-AND-PROCESS-CBL.                            ELSNOTE1
00544      IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00545         KTG-PAN-PARTICP-IND (KTG-IDX) NOT = ZEROS                 ELSNOTE1
00546         PERFORM BUILD-AND-PROCESS-PAN.                            ELSNOTE1
00547      IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00548            KTG-PARTICIPAT-PROV-OPTION (KTG-IDX) NOT = ZEROS       ELSNOTE1
00549         PERFORM BUILD-AND-PROCESS-PPO.                            ELSNOTE1
00550      IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00551            KTG-POS-PARTICP-IND (KTG-IDX) NOT = ZEROS              ELSNOTE1
00552         PERFORM BUILD-AND-PROCESS-MCN.                            ELSNOTE1
00553      IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00554             KTG-NEW-POS-IND (KTG-IDX) NOT = ZEROS                 ELSNOTE1
00555         PERFORM BUILD-AND-PROCESS-POS.                            ELSNOTE1
00556      IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00557             KTG-PRODUCT-TYPE (KTG-IDX) NOT = ZEROS                ELSNOTE1
00558         PERFORM BUILD-AND-PROCESS-PRODUCT-TYPE.                   ELSNOTE1
00559 *    IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00560 *           KTG-ALLIANCE-IND NOT = ZEROS                          ELSNOTE1
00561 *       PERFORM BUILD-AND-PROCESS-ALLIANCE-IND.                   ELSNOTE1
00562      IF KTG-SEL (KTG-IDX) AND                                     ELSNOTE1
00563             KTG-BLUE-SCRIPT (KTG-IDX) NOT = ZEROS                 ELSNOTE1
00564         PERFORM BUILD-AND-PROCESS-BLUE-SCRIPT.                    ELSNOTE1
00565                                                                   ELSNOTE1
00566      IF WS-ALLIANCE-NOT-DONE                                      ELSNOTE1
00567         IF KTG-SEL (KTG-IDX) AND                                  ELSNOTE1
00568             KTG-ALLIANCE-IND NOT = ZEROS                          ELSNOTE1
00569         PERFORM BUILD-AND-PROCESS-ALLIANCE-IND                    ELSNOTE1
00570         SET WS-ALLIANCE-DONE TO TRUE.                             ELSNOTE1
00571 ************************************************************      ELSNOTE1
00572 *                                                          *      ELSNOTE1
00573 *       BUILD AND PROCESS CPO                              *      ELSNOTE1
00574 *                                                          *      ELSNOTE1
00575 ************************************************************      ELSNOTE1
00576  BUILD-AND-PROCESS-CPO.                                           ELSNOTE1
00577      SET PROCESSING-CPO TO TRUE.                                  ELSNOTE1
00578      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00579      PERFORM BUILD-CPO-OPTION-KEY.                                ELSNOTE1
00580      EVALUATE TRUE                                                ELSNOTE1
00581         WHEN WS-GRP-CNT = ZERO                                    ELSNOTE1
00582           CONTINUE                                                ELSNOTE1
00583         WHEN WS-INIT = ZERO                                       ELSNOTE1
00584           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00585              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00586           END-IF                                                  ELSNOTE1
00587           MOVE 1 TO WS-INIT                                       ELSNOTE1
00588      END-EVALUATE.                                                ELSNOTE1
00589      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00590      INITIALIZE WS-INIT.                                          ELSNOTE1
00591                                                                   ELSNOTE1
00592 ************************************************************      ELSNOTE1
00593 *                                                          *      ELSNOTE1
00594 *       BUILD AND PROCESS CBL                              *      ELSNOTE1
00595 *                                                          *      ELSNOTE1
00596 ************************************************************      ELSNOTE1
00597  BUILD-AND-PROCESS-CBL.                                           ELSNOTE1
00598      SET PROCESSING-CBL TO TRUE.                                  ELSNOTE1
00599      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00600      PERFORM BUILD-CBL-OPTION-KEY.                                ELSNOTE1
00601      EVALUATE TRUE                                                ELSNOTE1
00602         WHEN WS-GRP-CNT = ZERO                                    ELSNOTE1
00603           CONTINUE                                                ELSNOTE1
00604         WHEN WS-INIT = ZERO                                       ELSNOTE1
00605           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00606              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00607           END-IF                                                  ELSNOTE1
00608           MOVE 1 TO WS-INIT                                       ELSNOTE1
00609      END-EVALUATE.                                                ELSNOTE1
00610      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00611      INITIALIZE WS-INIT.                                          ELSNOTE1
00612                                                                   ELSNOTE1
00613 ************************************************************      ELSNOTE1
00614 *                                                          *      ELSNOTE1
00615 *       BUILD AND PROCESS PAN                              *      ELSNOTE1
00616 *                                                          *      ELSNOTE1
00617 ************************************************************      ELSNOTE1
00618  BUILD-AND-PROCESS-PAN.                                           ELSNOTE1
00619      SET PROCESSING-PAN TO TRUE.                                  ELSNOTE1
00620      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00621      PERFORM BUILD-PAN-OPTION-KEY.                                ELSNOTE1
00622      EVALUATE TRUE                                                ELSNOTE1
00623         WHEN WS-GRP-CNT = ZERO                                    ELSNOTE1
00624           CONTINUE                                                ELSNOTE1
00625         WHEN WS-INIT = ZERO                                       ELSNOTE1
00626           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00627              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00628           END-IF                                                  ELSNOTE1
00629           MOVE 1 TO WS-INIT                                       ELSNOTE1
00630      END-EVALUATE.                                                ELSNOTE1
00631      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00632      INITIALIZE WS-INIT.                                          ELSNOTE1
00633                                                                   ELSNOTE1
00634 ************************************************************      ELSNOTE1
00635 *                                                          *      ELSNOTE1
00636 *       BUILD AND PROCESS RPO                              *      ELSNOTE1
00637 *                                                          *      ELSNOTE1
00638 ************************************************************      ELSNOTE1
00639  BUILD-AND-PROCESS-RPO.                                           ELSNOTE1
00640      SET PROCESSING-RPO TO TRUE.                                  ELSNOTE1
00641      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00642      PERFORM BUILD-RPO-OPTION-KEY.                                ELSNOTE1
00643      EVALUATE TRUE                                                ELSNOTE1
00644         WHEN WS-GRP-CNT = ZERO                                    ELSNOTE1
00645           CONTINUE                                                ELSNOTE1
00646         WHEN WS-INIT = ZERO                                       ELSNOTE1
00647           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00648              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00649           END-IF                                                  ELSNOTE1
00650           MOVE 1 TO WS-INIT                                       ELSNOTE1
00651      END-EVALUATE.                                                ELSNOTE1
00652      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00653      INITIALIZE WS-INIT.                                          ELSNOTE1
00654                                                                   ELSNOTE1
00655 ************************************************************      ELSNOTE1
00656 *                                                          *      ELSNOTE1
00657 *       BUILD AND PROCESS PPO                              *      ELSNOTE1
00658 *                                                          *      ELSNOTE1
00659 ************************************************************      ELSNOTE1
00660  BUILD-AND-PROCESS-PPO.                                           ELSNOTE1
00661      SET PROCESSING-PPO TO TRUE.                                  ELSNOTE1
00662      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00663      PERFORM BUILD-PPO-OPTION-KEY.                                ELSNOTE1
00664      EVALUATE TRUE                                                ELSNOTE1
00665         WHEN WS-GRP-CNT = ZERO                                    ELSNOTE1
00666           CONTINUE                                                ELSNOTE1
00667         WHEN WS-INIT = ZERO                                       ELSNOTE1
00668           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00669              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00670           END-IF                                                  ELSNOTE1
00671           MOVE 1 TO WS-INIT                                       ELSNOTE1
00672      END-EVALUATE.                                                ELSNOTE1
00673      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00674      INITIALIZE WS-INIT.                                          ELSNOTE1
00675                                                                   ELSNOTE1
00676 ************************************************************      ELSNOTE1
00677 *                                                          *      ELSNOTE1
00678 *       BUILD AND PROCESS POS                              *      ELSNOTE1
00679 *                                                          *      ELSNOTE1
00680 ************************************************************      ELSNOTE1
00681  BUILD-AND-PROCESS-POS.                                           ELSNOTE1
00682      SET PROCESSING-POS TO TRUE.                                  ELSNOTE1
00683      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00684      PERFORM BUILD-POS-OPTION-KEY.                                ELSNOTE1
00685      EVALUATE TRUE                                                ELSNOTE1
00686         WHEN WS-GRP-CNT = ZERO                                    ELSNOTE1
00687           CONTINUE                                                ELSNOTE1
00688         WHEN WS-INIT = ZERO                                       ELSNOTE1
00689           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00690              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00691           END-IF                                                  ELSNOTE1
00692           MOVE 1 TO WS-INIT                                       ELSNOTE1
00693      END-EVALUATE.                                                ELSNOTE1
00694      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00695      INITIALIZE WS-INIT.                                          ELSNOTE1
00696                                                                   ELSNOTE1
00697 ************************************************************      ELSNOTE1
00698 *                                                          *      ELSNOTE1
00699 *       BUILD AND PROCESS ALLIANCE IND                     *      ELSNOTE1
00700 *                                                          *      ELSNOTE1
00701 ************************************************************      ELSNOTE1
00702  BUILD-AND-PROCESS-ALLIANCE-IND.                                  ELSNOTE1
00703      SET PROCESSING-ALL TO TRUE.                                  ELSNOTE1
00704      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00705      MOVE KTG-ALLIANCE-IND                                        ELSNOTE1
00706                                  TO WS-HOLD-ALLNC-TYPE.           ELSNOTE1
00707      EVALUATE TRUE                                                ELSNOTE1
00708         WHEN WS-GRP-CNT = ZERO                                    ELSNOTE1
00709           CONTINUE                                                ELSNOTE1
00710         WHEN WS-INIT = ZERO                                       ELSNOTE1
00711           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00712              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00713           END-IF                                                  ELSNOTE1
00714           MOVE 1 TO WS-INIT                                       ELSNOTE1
00715      END-EVALUATE.                                                ELSNOTE1
00716      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00717      INITIALIZE WS-INIT.                                          ELSNOTE1
00718                                                                   ELSNOTE1
00719 ************************************************************      ELSNOTE1
00720 *                                                          *      ELSNOTE1
00721 *       BUILD AND PROCESS PRODUCT TYPE                     *      ELSNOTE1
00722 *                                                          *      ELSNOTE1
00723 ************************************************************      ELSNOTE1
00724  BUILD-AND-PROCESS-PRODUCT-TYPE.                                  ELSNOTE1
00725      SET PROCESSING-PRD TO TRUE.                                  ELSNOTE1
00726      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00727 *    MOVE KTG-PRODUCT-TYPE (KTG-IDX)                              ELSNOTE1
00728 *                                TO WS-HOLD-PRDCT-TYPE.           ELSNOTE1
00729      EVALUATE TRUE                                                ELSNOTE1
00730         WHEN WS-GRP-CNT = ZERO                                    ELSNOTE1
00731           CONTINUE                                                ELSNOTE1
00732         WHEN WS-INIT = ZERO                                       ELSNOTE1
00733           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00734              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00735           END-IF                                                  ELSNOTE1
00736           MOVE 1 TO WS-INIT                                       ELSNOTE1
00737      END-EVALUATE.                                                ELSNOTE1
00738      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00739      INITIALIZE WS-INIT.                                          ELSNOTE1
00740                                                                   ELSNOTE1
00741 ************************************************************      ELSNOTE1
00742 *                                                          *      ELSNOTE1
00743 *       BUILD AND PROCESS BLUE SCRIPT                      *      ELSNOTE1
00744 *                                                          *      ELSNOTE1
00745 ************************************************************      ELSNOTE1
00746  BUILD-AND-PROCESS-BLUE-SCRIPT.                                   ELSNOTE1
00747      SET PROCESSING-BLS TO TRUE.                                  ELSNOTE1
00748      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00749      EVALUATE TRUE                                                ELSNOTE1
00750         WHEN WS-GRP-CNT = ZERO                                    ELSNOTE1
00751           CONTINUE                                                ELSNOTE1
00752         WHEN WS-INIT = ZERO                                       ELSNOTE1
00753           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00754              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00755           END-IF                                                  ELSNOTE1
00756           MOVE 1 TO WS-INIT                                       ELSNOTE1
00757      END-EVALUATE.                                                ELSNOTE1
00758      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00759      INITIALIZE WS-INIT.                                          ELSNOTE1
00760                                                                   ELSNOTE1
00761 ************************************************************      ELSNOTE1
00762 *                                                          *      ELSNOTE1
00763 *       BUILD AND PROCESS MCN                              *      ELSNOTE1
00764 *                                                          *      ELSNOTE1
00765 ************************************************************      ELSNOTE1
00766  BUILD-AND-PROCESS-MCN.                                           ELSNOTE1
00767      SET PROCESSING-MCN TO TRUE.                                  ELSNOTE1
00768      MOVE 1 TO WS-GRP-CNT.                                        ELSNOTE1
00769      PERFORM BUILD-MCN-OPTION-KEY.                                ELSNOTE1
00770      EVALUATE TRUE                                                ELSNOTE1
00771        WHEN WS-GRP-CNT = ZERO                                     ELSNOTE1
00772           CONTINUE                                                ELSNOTE1
00773        WHEN WS-INIT = ZERO                                        ELSNOTE1
00774           IF MENU-2-NOT-INITIALIZED                               ELSNOTE1
00775              PERFORM INITIALIZE-MENU-2                            ELSNOTE1
00776           END-IF                                                  ELSNOTE1
00777           MOVE 1 TO WS-INIT                                       ELSNOTE1
00778      END-EVALUATE.                                                ELSNOTE1
00779      PERFORM CHECK-FOR-DUPLICATES.                                ELSNOTE1
00780      INITIALIZE WS-INIT.                                          ELSNOTE1
00781                                                                   ELSNOTE1
00782 ************************************************************      ELSNOTE1
00783 *                                                          *      ELSNOTE1
00784 *        BUILD PPO OPTION KEY                              *      ELSNOTE1
00785 *                                                          *      ELSNOTE1
00786 ************************************************************      ELSNOTE1
00787  BUILD-PPO-OPTION-KEY.                                            ELSNOTE1
00788      MOVE KTG-PARTICIPAT-PROV-OPTION (KTG-IDX)                    ELSNOTE1
00789           TO WS-PPO-KEY-OPTION                                    ELSNOTE1
00790              WS-TEMP-KEY-OPTION.                                  ELSNOTE1
00791      MOVE KTG-EFF-DT-CENTURY (KTG-IDX) TO WS-PPO-KEY-EFF-DT       ELSNOTE1
00792                                     WS-TEMP-KEY-EFF-DT.           ELSNOTE1
00793      MOVE KTG-TERM-DT-CENTURY    (KTG-IDX) TO                     ELSNOTE1
00794          WS-PPO-KEY-TERMN-DT                                      ELSNOTE1
00795          WS-TEMP-KEY-TERMN-DT.                                    ELSNOTE1
00796                                                                   ELSNOTE1
00797 ************************************************************      ELSNOTE1
00798 *                                                          *      ELSNOTE1
00799 *        BUILD CPO OPTION KEY                              *      ELSNOTE1
00800 *                                                          *      ELSNOTE1
00801 ************************************************************      ELSNOTE1
00802  BUILD-CPO-OPTION-KEY.                                            ELSNOTE1
00803      MOVE KTG-CPO-PARTICP-IND (KTG-IDX)                           ELSNOTE1
00804           TO WS-CPO-KEY-OPTION                                    ELSNOTE1
00805              WS-TEMP-KEY-OPTION.                                  ELSNOTE1
00806      MOVE KTG-EFF-DT-CENTURY  (KTG-IDX) TO WS-CPO-KEY-EFF-DT      ELSNOTE1
00807                                           WS-TEMP-KEY-EFF-DT.     ELSNOTE1
00808      MOVE KTG-TERM-DT-CENTURY      (KTG-IDX) TO                   ELSNOTE1
00809          WS-CPO-KEY-TERMN-DT                                      ELSNOTE1
00810          WS-TEMP-KEY-TERMN-DT.                                    ELSNOTE1
00811                                                                   ELSNOTE1
00812                                                                   ELSNOTE1
00813 ************************************************************      ELSNOTE1
00814 *                                                          *      ELSNOTE1
00815 *        BUILD CBL OPTION KEY                              *      ELSNOTE1
00816 *                                                          *      ELSNOTE1
00817 ************************************************************      ELSNOTE1
00818  BUILD-CBL-OPTION-KEY.                                            ELSNOTE1
00819      MOVE KTG-CBL-PARTICP-IND (KTG-IDX)                           ELSNOTE1
00820           TO WS-CBL-KEY-OPTION                                    ELSNOTE1
00821              WS-TEMP-KEY-OPTION.                                  ELSNOTE1
00822      MOVE KTG-EFF-DT-CENTURY (KTG-IDX) TO WS-CBL-KEY-EFF-DT       ELSNOTE1
00823                                     WS-TEMP-KEY-EFF-DT.           ELSNOTE1
00824      MOVE KTG-TERM-DT-CENTURY (KTG-IDX) TO                        ELSNOTE1
00825          WS-CBL-KEY-TERMN-DT                                      ELSNOTE1
00826          WS-TEMP-KEY-TERMN-DT.                                    ELSNOTE1
00827                                                                   ELSNOTE1
00828 ************************************************************      ELSNOTE1
00829 *                                                          *      ELSNOTE1
00830 *        BUILD PAN OPTION KEY                              *      ELSNOTE1
00831 *                                                          *      ELSNOTE1
00832 ************************************************************      ELSNOTE1
00833  BUILD-PAN-OPTION-KEY.                                            ELSNOTE1
00834      MOVE KTG-PAN-PARTICP-IND (KTG-IDX)                           ELSNOTE1
00835           TO WS-PAN-KEY-OPTION                                    ELSNOTE1
00836              WS-TEMP-KEY-OPTION.                                  ELSNOTE1
00837      MOVE KTG-EFF-DT-CENTURY (KTG-IDX) TO WS-PAN-KEY-EFF-DT       ELSNOTE1
00838                                     WS-TEMP-KEY-EFF-DT.           ELSNOTE1
00839      MOVE KTG-TERM-DT-CENTURY     (KTG-IDX) TO                    ELSNOTE1
00840          WS-PAN-KEY-TERMN-DT                                      ELSNOTE1
00841          WS-TEMP-KEY-TERMN-DT.                                    ELSNOTE1
00842                                                                   ELSNOTE1
00843 ************************************************************      ELSNOTE1
00844 *                                                          *      ELSNOTE1
00845 *        BUILD RPO OPTION KEY                              *      ELSNOTE1
00846 *                                                          *      ELSNOTE1
00847 ************************************************************      ELSNOTE1
00848  BUILD-RPO-OPTION-KEY.                                            ELSNOTE1
00849      MOVE KTG-RPO-PARTICP-IND (KTG-IDX)                           ELSNOTE1
00850           TO WS-RPO-KEY-OPTION                                    ELSNOTE1
00851              WS-TEMP-KEY-OPTION.                                  ELSNOTE1
00852      MOVE KTG-EFF-DT-CENTURY   (KTG-IDX) TO WS-RPO-KEY-EFF-DT     ELSNOTE1
00853                                     WS-TEMP-KEY-EFF-DT.           ELSNOTE1
00854      MOVE KTG-TERM-DT-CENTURY     (KTG-IDX) TO                    ELSNOTE1
00855          WS-RPO-KEY-TERMN-DT                                      ELSNOTE1
00856          WS-TEMP-KEY-TERMN-DT.                                    ELSNOTE1
00857                                                                   ELSNOTE1
00858 ************************************************************      ELSNOTE1
00859 *                                                          *      ELSNOTE1
00860 *        BUILD MCN OPTION KEY                              *      ELSNOTE1
00861 *                                                          *      ELSNOTE1
00862 ************************************************************      ELSNOTE1
00863  BUILD-MCN-OPTION-KEY.                                            ELSNOTE1
00864      MOVE KTG-POS-PARTICP-IND (KTG-IDX)                           ELSNOTE1
00865           TO WS-MCN-KEY-OPTION                                    ELSNOTE1
00866              WS-TEMP-KEY-OPTION.                                  ELSNOTE1
00867      MOVE KTG-EFF-DT-CENTURY  (KTG-IDX) TO WS-MCN-KEY-EFF-DT      ELSNOTE1
00868                                     WS-TEMP-KEY-EFF-DT.           ELSNOTE1
00869      MOVE KTG-TERM-DT-CENTURY          (KTG-IDX) TO               ELSNOTE1
00870          WS-MCN-KEY-TERMN-DT                                      ELSNOTE1
00871          WS-TEMP-KEY-TERMN-DT.                                    ELSNOTE1
00872                                                                   ELSNOTE1
00873 ************************************************************      ELSNOTE1
00874 *                                                          *      ELSNOTE1
00875 *        BUILD POS OPTION KEY                              *      ELSNOTE1
00876 *                                                          *      ELSNOTE1
00877 ************************************************************      ELSNOTE1
00878  BUILD-POS-OPTION-KEY.                                            ELSNOTE1
00879      MOVE KTG-NEW-POS-IND (KTG-IDX)                               ELSNOTE1
00880           TO WS-POS-KEY-OPTION                                    ELSNOTE1
00881              WS-TEMP-KEY-OPTION.                                  ELSNOTE1
00882      MOVE KTG-EFF-DT-CENTURY   (KTG-IDX) TO WS-POS-KEY-EFF-DT     ELSNOTE1
00883                                     WS-TEMP-KEY-EFF-DT.           ELSNOTE1
00884      MOVE KTG-TERM-DT-CENTURY (KTG-IDX) TO                        ELSNOTE1
00885          WS-POS-KEY-TERMN-DT                                      ELSNOTE1
00886          WS-TEMP-KEY-TERMN-DT.                                    ELSNOTE1
00887                                                                   ELSNOTE1
00888 ************************************************************      ELSNOTE1
00889 *                                                          *      ELSNOTE1
00890 *        INITIALIZE MENU-2                                 *      ELSNOTE1
00891 *                                                          *      ELSNOTE1
00892 ************************************************************      ELSNOTE1
00893  INITIALIZE-MENU-2.                                               ELSNOTE1
00894      MOVE LOW-VALUES TO TCAR-COMPRESSION-WORK-AREA.               ELSNOTE1
00895      MOVE ZEROS TO TCAR-TO-SUB                                    ELSNOTE1
00896      MOVE 79 TO  TCAR-OUTPUT-FIELD-COUNT.                         ELSNOTE1
00897      MOVE 79 TO  TCAR-AREA-LENGTH.                                ELSNOTE1
00898      MOVE LENGTH OF WS-SCREEN-TITLE-1 TO                          ELSNOTE1
00899                  TCAR-OUTPUT-FIELD-1-LEN.                         ELSNOTE1
00900      MOVE WS-SCREEN-TITLE-1 TO TCAR-FROM-LINE (1).                ELSNOTE1
00901                                                                   ELSNOTE1
00902      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELSNOTE1
00903      PERFORM TCPR-000-TEXT-UNSTRING.                              ELSNOTE1
00904                                                                   ELSNOTE1
00905      MOVE TCAR-OPF-DATA (1)                                       ELSNOTE1
00906           TO MHD-HDG-LINE (1).                                    ELSNOTE1
00907      MOVE WS-NUM-HEADING-LINES                                    ELSNOTE1
00908          TO MHD-NBR-HDG-LINES.                                    ELSNOTE1
00909      MOVE WS-PPO-MENU-TITLE TO SSB-MNU-TITLE.                     ELSNOTE1
00910 *    MOVE WS-SCREEN-TITLE-1 TO MHD-HDG-LINE (1).                  ELSNOTE1
00911      MOVE SPACES            TO MHD-HDG-LINE (2).                  ELSNOTE1
00912      MOVE LOW-VALUES TO TCAR-COMPRESSION-WORK-AREA.               ELSNOTE1
00913                                                                   ELSNOTE1
00914      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSNOTE1
00915      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSNOTE1
00916            WS-DUMMY-PTR.                                          ELSNOTE1
00917                                                                   ELSNOTE1
00918      SET IOP-GETMAIN-REC TO TRUE.                                 ELSNOTE1
00919      COMPUTE                                                      ELSNOTE1
00920         IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES +             ELSNOTE1
00921                 LENGTH OF MSD-DESCR-LINE *                        ELSNOTE1
00922                         CIA-MVO.                                  ELSNOTE1
00923      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSNOTE1
00924      SET CIA-STG-GETMAIN TO TRUE.                                 ELSNOTE1
00925      EXEC CICS LINK PROGRAM('ELUSTGMG')                           ELSNOTE1
00926                COMMAREA(DFHCOMMAREA)                              ELSNOTE1
00927                 END-EXEC.                                         ELSNOTE1
00928                                                                   ELSNOTE1
00929      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS                    ELSNOTE1
00930          TO IOP-REC-PTR.                                          ELSNOTE1
00931      SET MENU-2-INITIALIZED TO TRUE.                              ELSNOTE1
00932                                                                   ELSNOTE1
00933      EJECT                                                        ELSNOTE1
00934 ************************************************************      ELSNOTE1
00935 *                                                          *      ELSNOTE1
00936 *        CHECK FOR DUPLICATES                              *      ELSNOTE1
00937 *                                                          *      ELSNOTE1
00938 ************************************************************      ELSNOTE1
00939  CHECK-FOR-DUPLICATES.                                            ELSNOTE1
00940 *           PERFORM CHECK-FOR-MENU-ITEM-DUPS                      ELSNOTE1
00941             IF WS-NO-DUPLICATE                                    ELSNOTE1
00942                PERFORM FORMAT-ITEM-DESCRIPTION.                   ELSNOTE1
00943                                                                   ELSNOTE1
00944 ************************************************************      ELSNOTE1
00945 *                                                          *      ELSNOTE1
00946 *        SETUP MENU OPTION                                 *      ELSNOTE1
00947 *                                                          *      ELSNOTE1
00948 ************************************************************      ELSNOTE1
00949  SETUP-MENU-OPTION.                                               ELSNOTE1
00950      ADD 1 TO MSO-NBR-MENU-OPTS.                                  ELSNOTE1
00951      SET MSO-IDX TO MSO-NBR-MENU-OPTS.                            ELSNOTE1
00952      MOVE SPACES TO  MSO-OPT-SEL (MSO-IDX).                       ELSNOTE1
00953      MOVE WS-TEMP-KEY TO  MSO-OPT-KWD (MSO-IDX).                  ELSNOTE1
00954                                                                   ELSNOTE1
00955      EJECT                                                        ELSNOTE1
00956 ************************************************************      ELSNOTE1
00957 *                                                          *      ELSNOTE1
00958 *        FORMAT ITEM DESCRIPTION                           *      ELSNOTE1
00959 *                                                          *      ELSNOTE1
00960 ************************************************************      ELSNOTE1
00961  FORMAT-ITEM-DESCRIPTION.                                         ELSNOTE1
00962      MOVE 'GROUP   '              TO CMF-RECORD-PREFIX.           ELSNOTE1
00963      MOVE LOW-VALUES TO TCAR-COMPRESSION-WORK-AREA.               ELSNOTE1
00964      MOVE ZERO TO TCAR-TO-SUB.                                    ELSNOTE1
00965      IF PROCESSING-RPO                                            ELSNOTE1
00966         PERFORM RPO-FORMAT-ITEM-DES.                              ELSNOTE1
00967      IF PROCESSING-CBL                                            ELSNOTE1
00968         PERFORM CBL-FORMAT-ITEM-DES.                              ELSNOTE1
00969      IF PROCESSING-CPO                                            ELSNOTE1
00970         PERFORM CPO-FORMAT-ITEM-DES.                              ELSNOTE1
00971      IF PROCESSING-MCN                                            ELSNOTE1
00972         PERFORM MCN-FORMAT-ITEM-DES.                              ELSNOTE1
00973      IF PROCESSING-PAN                                            ELSNOTE1
00974         PERFORM PAN-FORMAT-ITEM-DES.                              ELSNOTE1
00975      IF PROCESSING-POS                                            ELSNOTE1
00976         PERFORM POS-FORMAT-ITEM-DES.                              ELSNOTE1
00977      IF PROCESSING-PPO                                            ELSNOTE1
00978         PERFORM PPO-FORMAT-ITEM-DES.                              ELSNOTE1
00979      IF PROCESSING-PRD                                            ELSNOTE1
00980         PERFORM PRD-FORMAT-ITEM-DES.                              ELSNOTE1
00981      IF PROCESSING-ALL                                            ELSNOTE1
00982         PERFORM ALL-FORMAT-ITEM-DES.                              ELSNOTE1
00983      IF PROCESSING-BLS                                            ELSNOTE1
00984         PERFORM BLS-FORMAT-ITEM-DES.                              ELSNOTE1
00985                                                                   ELSNOTE1
00986 ************************************************************      ELSNOTE1
00987 *                                                          *      ELSNOTE1
00988 *        PRD FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
00989 *                                                          *      ELSNOTE1
00990 ************************************************************      ELSNOTE1
00991  PRD-FORMAT-ITEM-DES.                                             ELSNOTE1
00992      IF PROCESSING-PRD AND WS-BEN-DISPLAYED                       ELSNOTE1
00993         CONTINUE                                                  ELSNOTE1
00994      ELSE                                                         ELSNOTE1
00995         IF WS-NO-DATES-DISPLAYED                                  ELSNOTE1
00996            PERFORM FORMAT-DATES                                   ELSNOTE1
00997         ELSE                                                      ELSNOTE1
00998            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
00999         END-IF                                                    ELSNOTE1
01000         PERFORM SETUP-MENU-OPTION                                 ELSNOTE1
01001         MOVE WS-BNFT-PRVSN-TPC1 TO TCAR-FROM-LINE (TCAR-TO-SUB)   ELSNOTE1
01002         ADD 1 TO TCAR-TO-SUB                                      ELSNOTE1
01003         MOVE WS-BNFT-PRVSN-TPC2 TO TCAR-FROM-LINE (TCAR-TO-SUB)   ELSNOTE1
01004         PERFORM PREPARE-AND-OUTPUT-TRANS                          ELSNOTE1
01005         SET WS-BEN-DISPLAYED TO TRUE                              ELSNOTE1
01006      END-IF.                                                      ELSNOTE1
01007                                                                   ELSNOTE1
01008                                                                   ELSNOTE1
01009 ************************************************************      ELSNOTE1
01010 *                                                          *      ELSNOTE1
01011 *        ALL FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
01012 *                                                          *      ELSNOTE1
01013 ************************************************************      ELSNOTE1
01014  ALL-FORMAT-ITEM-DES.                                             ELSNOTE1
01015      IF PROCESSING-ALL AND WS-BEN-DISPLAYED                       ELSNOTE1
01016         CONTINUE                                                  ELSNOTE1
01017      ELSE                                                         ELSNOTE1
01018         IF WS-NO-DATES-DISPLAYED                                  ELSNOTE1
01019            PERFORM FORMAT-DATES                                   ELSNOTE1
01020         ELSE                                                      ELSNOTE1
01021            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
01022         END-IF                                                    ELSNOTE1
01023         PERFORM SETUP-MENU-OPTION                                 ELSNOTE1
01024         MOVE WS-ALLIANCE-TOPIC TO TCAR-FROM-LINE (TCAR-TO-SUB)    ELSNOTE1
01025         PERFORM PREPARE-AND-OUTPUT-TRANS                          ELSNOTE1
01026         SET WS-BEN-DISPLAYED TO TRUE                              ELSNOTE1
01027      END-IF.                                                      ELSNOTE1
01028                                                                   ELSNOTE1
01029 ************************************************************      ELSNOTE1
01030 *                                                          *      ELSNOTE1
01031 *        BLS FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
01032 * BLUE SCRIPT                                              *      ELSNOTE1
01033 ************************************************************      ELSNOTE1
01034  BLS-FORMAT-ITEM-DES.                                             ELSNOTE1
01035      IF PROCESSING-BLS AND WS-BEN-DISPLAYED                       ELSNOTE1
01036         CONTINUE                                                  ELSNOTE1
01037      ELSE                                                         ELSNOTE1
01038         IF WS-NO-DATES-DISPLAYED                                  ELSNOTE1
01039            PERFORM FORMAT-DATES                                   ELSNOTE1
01040         ELSE                                                      ELSNOTE1
01041            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
01042         END-IF                                                    ELSNOTE1
01043         PERFORM SETUP-MENU-OPTION                                 ELSNOTE1
01044         MOVE WS-BLUE-SCRIPT-TPC1 TO TCAR-FROM-LINE (TCAR-TO-SUB)  ELSNOTE1
01045         ADD 1 TO TCAR-TO-SUB                                      ELSNOTE1
01046         MOVE WS-BLUE-SCRIPT-TPC2 TO TCAR-FROM-LINE (TCAR-TO-SUB)  ELSNOTE1
01047         PERFORM PREPARE-AND-OUTPUT-TRANS                          ELSNOTE1
01048      END-IF.                                                      ELSNOTE1
01049                                                                   ELSNOTE1
01050 ************************************************************      ELSNOTE1
01051 *                                                          *      ELSNOTE1
01052 *        PPO FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
01053 *                                                          *      ELSNOTE1
01054 ************************************************************      ELSNOTE1
01055  PPO-FORMAT-ITEM-DES.                                             ELSNOTE1
01056      PERFORM SETUP-MENU-OPTION.                                   ELSNOTE1
01057      MOVE 'PARTICIPAT-PROV-OPTION'                                ELSNOTE1
01058                    TO CMF-ELEMENT-SYSTEM-NAME.                    ELSNOTE1
01059      MOVE KTG-PARTICIPAT-PROV-OPTION (KTG-IDX)                    ELSNOTE1
01060                                   TO CMF-CODE-VALUE               ELSNOTE1
01061                                     WS-NON-STANDARD-VALUES.       ELSNOTE1
01062      PERFORM MOVE-OPTIONS.                                        ELSNOTE1
01063      IF WS-NON-STANDARD                                           ELSNOTE1
01064          ADD 1 TO TCAR-TO-SUB                                     ELSNOTE1
01065          MOVE WS-PPO-TOPIC-REF-1 TO TCAR-FROM-LINE                ELSNOTE1
01066               (TCAR-TO-SUB)                                       ELSNOTE1
01067      END-IF.                                                      ELSNOTE1
01068      PERFORM PREPARE-AND-OUTPUT-TRANS.                            ELSNOTE1
01069                                                                   ELSNOTE1
01070 ************************************************************      ELSNOTE1
01071 *                                                          *      ELSNOTE1
01072 *        CBL FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
01073 *                                                          *      ELSNOTE1
01074 ************************************************************      ELSNOTE1
01075  CBL-FORMAT-ITEM-DES.                                             ELSNOTE1
01076      PERFORM SETUP-MENU-OPTION                                    ELSNOTE1
01077      MOVE 'CBL-PARTICIPATION-IND'                                 ELSNOTE1
01078                          TO CMF-ELEMENT-SYSTEM-NAME.              ELSNOTE1
01079      MOVE KTG-CBL-PARTICP-IND (KTG-IDX)                           ELSNOTE1
01080                                   TO CMF-CODE-VALUE               ELSNOTE1
01081                                     WS-NON-STANDARD-CBL-VALUES.   ELSNOTE1
01082      PERFORM MOVE-OPTIONS.                                        ELSNOTE1
01083      IF WS-NON-STANDARD-CBL                                       ELSNOTE1
01084          ADD 1 TO TCAR-TO-SUB                                     ELSNOTE1
01085          MOVE WS-CBL-TOPIC-REF-1 TO TCAR-FROM-LINE                ELSNOTE1
01086             (TCAR-TO-SUB)                                         ELSNOTE1
01087          ADD 1 TO TCAR-TO-SUB                                     ELSNOTE1
01088          MOVE WS-CBL-TOPIC-REF-2                                  ELSNOTE1
01089                   TO TCAR-FROM-LINE (TCAR-TO-SUB)                 ELSNOTE1
01090      END-IF.                                                      ELSNOTE1
01091      PERFORM PREPARE-AND-OUTPUT-TRANS.                            ELSNOTE1
01092                                                                   ELSNOTE1
01093 ************************************************************      ELSNOTE1
01094 *                                                          *      ELSNOTE1
01095 *        RPO FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
01096 *                                                          *      ELSNOTE1
01097 ************************************************************      ELSNOTE1
01098  RPO-FORMAT-ITEM-DES.                                             ELSNOTE1
01099      PERFORM SETUP-MENU-OPTION                                    ELSNOTE1
01100      MOVE 'RPO-INDICATOR' TO CMF-ELEMENT-SYSTEM-NAME.             ELSNOTE1
01101      MOVE KTG-RPO-PARTICP-IND (KTG-IDX)                           ELSNOTE1
01102                                   TO CMF-CODE-VALUE               ELSNOTE1
01103                                     WS-NON-STANDARD-RPO-VALUES.   ELSNOTE1
01104      PERFORM MOVE-OPTIONS.                                        ELSNOTE1
01105      IF WS-NON-STANDARD-RPO                                       ELSNOTE1
01106          ADD 1 TO TCAR-TO-SUB                                     ELSNOTE1
01107          MOVE WS-RPO-TOPIC-REF-1 TO TCAR-FROM-LINE                ELSNOTE1
01108             (TCAR-TO-SUB)                                         ELSNOTE1
01109          ADD 1 TO TCAR-TO-SUB                                     ELSNOTE1
01110          MOVE WS-RPO-TOPIC-REF-2                                  ELSNOTE1
01111                   TO TCAR-FROM-LINE (TCAR-TO-SUB)                 ELSNOTE1
01112      END-IF.                                                      ELSNOTE1
01113      PERFORM PREPARE-AND-OUTPUT-TRANS.                            ELSNOTE1
01114                                                                   ELSNOTE1
01115 ************************************************************      ELSNOTE1
01116 *                                                          *      ELSNOTE1
01117 *        CPO FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
01118 *                                                          *      ELSNOTE1
01119 ************************************************************      ELSNOTE1
01120  CPO-FORMAT-ITEM-DES.                                             ELSNOTE1
01121      PERFORM SETUP-MENU-OPTION                                    ELSNOTE1
01122      MOVE 'CPO-PARTICIPATION-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELSNOTE1
01123      MOVE KTG-CPO-PARTICP-IND (KTG-IDX)                           ELSNOTE1
01124                                   TO CMF-CODE-VALUE               ELSNOTE1
01125                                     WS-NON-STANDARD-CPO-VALUES.   ELSNOTE1
01126      PERFORM MOVE-OPTIONS.                                        ELSNOTE1
01127      IF WS-NON-STANDARD-CPO                                       ELSNOTE1
01128          ADD 1 TO TCAR-TO-SUB                                     ELSNOTE1
01129          MOVE WS-CPO-TOPIC-REF-1 TO TCAR-FROM-LINE                ELSNOTE1
01130             (TCAR-TO-SUB)                                         ELSNOTE1
01131          ADD 1 TO TCAR-TO-SUB                                     ELSNOTE1
01132          MOVE WS-CPO-TOPIC-REF-2                                  ELSNOTE1
01133                   TO TCAR-FROM-LINE (TCAR-TO-SUB)                 ELSNOTE1
01134      END-IF.                                                      ELSNOTE1
01135      PERFORM PREPARE-AND-OUTPUT-TRANS.                            ELSNOTE1
01136                                                                   ELSNOTE1
01137 ************************************************************      ELSNOTE1
01138 *                                                          *      ELSNOTE1
01139 *        PAN FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
01140 *                                                          *      ELSNOTE1
01141 ************************************************************      ELSNOTE1
01142  PAN-FORMAT-ITEM-DES.                                             ELSNOTE1
01143      PERFORM SETUP-MENU-OPTION                                    ELSNOTE1
01144      MOVE 'PAN-PARTICIPATION-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELSNOTE1
01145      MOVE KTG-PAN-PARTICP-IND (KTG-IDX)                           ELSNOTE1
01146                                   TO CMF-CODE-VALUE               ELSNOTE1
01147                                     WS-NON-STANDARD-PAN-VALUES.   ELSNOTE1
01148      PERFORM MOVE-OPTIONS.                                        ELSNOTE1
01149      IF WS-NON-STANDARD-PAN                                       ELSNOTE1
01150          ADD 1 TO TCAR-TO-SUB                                     ELSNOTE1
01151          MOVE WS-PAN-TOPIC-REF-1 TO TCAR-FROM-LINE                ELSNOTE1
01152             (TCAR-TO-SUB)                                         ELSNOTE1
01153          ADD 1 TO TCAR-TO-SUB                                     ELSNOTE1
01154          MOVE WS-PAN-TOPIC-REF-2                                  ELSNOTE1
01155                   TO TCAR-FROM-LINE (TCAR-TO-SUB)                 ELSNOTE1
01156      END-IF.                                                      ELSNOTE1
01157      PERFORM PREPARE-AND-OUTPUT-TRANS.                            ELSNOTE1
01158                                                                   ELSNOTE1
01159 ************************************************************      ELSNOTE1
01160 *                                                          *      ELSNOTE1
01161 *        MCN FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
01162 *                                                          *      ELSNOTE1
01163 ************************************************************      ELSNOTE1
01164  MCN-FORMAT-ITEM-DES.                                             ELSNOTE1
01165      PERFORM SETUP-MENU-OPTION                                    ELSNOTE1
01166      MOVE 'POS-PARTICP-IND' TO CMF-ELEMENT-SYSTEM-NAME            ELSNOTE1
01167      MOVE KTG-POS-PARTICP-IND (KTG-IDX)                           ELSNOTE1
01168                                 TO CMF-CODE-VALUE                 ELSNOTE1
01169                                     WS-NON-STANDARD-VALUES.       ELSNOTE1
01170      PERFORM MOVE-OPTIONS.                                        ELSNOTE1
01171      PERFORM PREPARE-AND-OUTPUT-TRANS.                            ELSNOTE1
01172                                                                   ELSNOTE1
01173 ************************************************************      ELSNOTE1
01174 *                                                          *      ELSNOTE1
01175 *        POS FORMAT ITEM DESCRIPTION                       *      ELSNOTE1
01176 *                                                          *      ELSNOTE1
01177 ************************************************************      ELSNOTE1
01178  POS-FORMAT-ITEM-DES.                                             ELSNOTE1
01179      PERFORM SETUP-MENU-OPTION                                    ELSNOTE1
01180      MOVE 'NEW-POS-IND' TO CMF-ELEMENT-SYSTEM-NAME.               ELSNOTE1
01181      MOVE KTG-NEW-POS-IND (KTG-IDX)                               ELSNOTE1
01182                                  TO CMF-CODE-VALUE                ELSNOTE1
01183                                     WS-NON-STANDARD-VALUES.       ELSNOTE1
01184      PERFORM MOVE-OPTIONS.                                        ELSNOTE1
01185      PERFORM PREPARE-AND-OUTPUT-TRANS.                            ELSNOTE1
01186                                                                   ELSNOTE1
01187 ************************************************************      ELSNOTE1
01188 *                                                          *      ELSNOTE1
01189 *        PREPARE AND OUTPUT TRANSLATION                    *      ELSNOTE1
01190 *                                                          *      ELSNOTE1
01191 ************************************************************      ELSNOTE1
01192  PREPARE-AND-OUTPUT-TRANS.                                        ELSNOTE1
01193      COMPUTE TCAR-AREA-LENGTH = TCAR-TO-SUB  *                    ELSNOTE1
01194              LENGTH TCAR-FROM-LINE.                               ELSNOTE1
01195      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSNOTE1
01196      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSNOTE1
01197            WS-DUMMY-PTR.                                          ELSNOTE1
01198                                                                   ELSNOTE1
01199      MOVE CIA-MVO                                                 ELSNOTE1
01200            TO  TCAR-OUTPUT-FIELD-COUNT.                           ELSNOTE1
01201      IF WS-NO-DATES-DISPLAYED                                     ELSNOTE1
01202         MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                       ELSNOTE1
01203         MOVE +51 TO TCAR-OUTPUT-FIELD-2-LEN                       ELSNOTE1
01204                     TCAR-OUTPUT-FIELD-3-LEN                       ELSNOTE1
01205      ELSE                                                         ELSNOTE1
01206         MOVE +51 TO TCAR-OUTPUT-FIELD-1-LEN                       ELSNOTE1
01207                     TCAR-OUTPUT-FIELD-2-LEN                       ELSNOTE1
01208                     TCAR-OUTPUT-FIELD-3-LEN                       ELSNOTE1
01209      END-IF.                                                      ELSNOTE1
01210                                                                   ELSNOTE1
01211      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELSNOTE1
01212      PERFORM TCPR-000-TEXT-UNSTRING.                              ELSNOTE1
01213                                                                   ELSNOTE1
01214      MOVE TCAR-OUTPUT-FIELDS-USED                                 ELSNOTE1
01215          TO MSD-NBR-DESCR-LINES.                                  ELSNOTE1
01216                                                                   ELSNOTE1
01217      IF WS-NO-DATES-DISPLAYED                                     ELSNOTE1
01218         PERFORM DISPLAY-WITH-DATES                                ELSNOTE1
01219      ELSE                                                         ELSNOTE1
01220         PERFORM DISPLAY-WITH-NO-DATES                             ELSNOTE1
01221      END-IF.                                                      ELSNOTE1
01222      SET WS-HOLD-IDX TO KTG-IDX.                                  ELSNOTE1
01223      MOVE KTG-EFF-DT (KTG-IDX) TO WS-HOLD-EFF-DT.                 ELSNOTE1
01224      MOVE KTG-TERMN-DT (KTG-IDX) TO WS-HOLD-TRMN-DT.              ELSNOTE1
01225      IF PROCESSING-PRD AND WS-BEN-DISPLAYED                       ELSNOTE1
01226         CONTINUE                                                  ELSNOTE1
01227      ELSE                                                         ELSNOTE1
01228         SET CIA-ELSMENU-DDN TO TRUE                               ELSNOTE1
01229         SET IOP-ADD TO TRUE                                       ELSNOTE1
01230         SET IOP-FCQ-NONE TO TRUE                                  ELSNOTE1
01231         EXEC CICS LINK                                            ELSNOTE1
01232                   PROGRAM('ELUIOPGM')                             ELSNOTE1
01233                   COMMAREA(DFHCOMMAREA)                           ELSNOTE1
01234                   END-EXEC                                        ELSNOTE1
01235      END-IF.                                                      ELSNOTE1
01236      SET WS-NOT-FIRST-DISPLAY TO TRUE.                            ELSNOTE1
01237      EJECT                                                        ELSNOTE1
01238 ************************************************************      ELSNOTE1
01239 *                                                          *      ELSNOTE1
01240 *      DISPLAY OUTPUT WITH DATES DISPLAYING                *      ELSNOTE1
01241 *                                                          *      ELSNOTE1
01242 ************************************************************      ELSNOTE1
01243  DISPLAY-WITH-DATES.                                              ELSNOTE1
01244         SET MSD-IDX TO 1.                                         ELSNOTE1
01245         SET WS-IDX TO 1.                                          ELSNOTE1
01246         MOVE TCAR-OPF-DATA (1)                                    ELSNOTE1
01247              TO WS-DETAIL-LINE-1.                                 ELSNOTE1
01248         SET WS-DATES-DISPLAYED TO TRUE.                           ELSNOTE1
01249         PERFORM PREPARE-TEXT-LINES                                ELSNOTE1
01250             VARYING TCAR-TO-SUB FROM 2 BY 1                       ELSNOTE1
01251                     UNTIL TCAR-TO-SUB >                           ELSNOTE1
01252                 TCAR-OUTPUT-FIELDS-USED.                          ELSNOTE1
01253         MOVE WS-DETAIL-LINE-1                                     ELSNOTE1
01254              TO MSD-DESCR-LINE (1).                               ELSNOTE1
01255         SET WS-IDX TO 1.                                          ELSNOTE1
01256         PERFORM MOVE-TEXT-LINES                                   ELSNOTE1
01257             VARYING MSD-IDX  FROM 2 BY 1                          ELSNOTE1
01258                    UNTIL MSD-IDX >                                ELSNOTE1
01259                 TCAR-OUTPUT-FIELDS-USED.                          ELSNOTE1
01260                                                                   ELSNOTE1
01261 ************************************************************      ELSNOTE1
01262 *                                                          *      ELSNOTE1
01263 *      DISPLAY OUTPUT WITH NO DATES DISPLAYING             *      ELSNOTE1
01264 *                                                          *      ELSNOTE1
01265 ************************************************************      ELSNOTE1
01266  DISPLAY-WITH-NO-DATES.                                           ELSNOTE1
01267         SET MSD-IDX TO 1.                                         ELSNOTE1
01268         SET WS-IDX TO 1.                                          ELSNOTE1
01269      PERFORM PREPARE-TEXT-LINES                                   ELSNOTE1
01270          VARYING TCAR-TO-SUB FROM 1 BY 1                          ELSNOTE1
01271                  UNTIL TCAR-TO-SUB >                              ELSNOTE1
01272              TCAR-OUTPUT-FIELDS-USED.                             ELSNOTE1
01273         SET WS-IDX TO 1.                                          ELSNOTE1
01274         PERFORM MOVE-TEXT-LINES                                   ELSNOTE1
01275             VARYING MSD-IDX  FROM 1 BY 1                          ELSNOTE1
01276                    UNTIL MSD-IDX >                                ELSNOTE1
01277                 TCAR-OUTPUT-FIELDS-USED.                          ELSNOTE1
01278                                                                   ELSNOTE1
01279 ************************************************************      ELSNOTE1
01280 *                                                          *      ELSNOTE1
01281 *        MOVE OPTIONS                                      *      ELSNOTE1
01282 *                                                          *      ELSNOTE1
01283 ************************************************************      ELSNOTE1
01284  MOVE-OPTIONS.                                                    ELSNOTE1
01285      EXEC CICS LINK                                               ELSNOTE1
01286                PROGRAM('ELUCMIF')                                 ELSNOTE1
01287                COMMAREA(DFHCOMMAREA)                              ELSNOTE1
01288                END-EXEC.                                          ELSNOTE1
01289      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELSNOTE1
01290      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSNOTE1
01291          ADDRESS OF CMF-DESCR.                                    ELSNOTE1
01292                                                                   ELSNOTE1
01293      IF KTG-EFF-DT-CENTURY (KTG-IDX) = WS-HOLD-EFF-DT             ELSNOTE1
01294        AND                                                        ELSNOTE1
01295          KTG-TERM-DT-CENTURY     (KTG-IDX) = WS-HOLD-TRMN-DT      ELSNOTE1
01296             SET WS-DATES-DISPLAYED TO TRUE                        ELSNOTE1
01297      END-IF.                                                      ELSNOTE1
01298      IF WS-NO-DATES-DISPLAYED                                     ELSNOTE1
01299         PERFORM FORMAT-DATES                                      ELSNOTE1
01300      ELSE                                                         ELSNOTE1
01301         ADD 1 TO TCAR-TO-SUB                                      ELSNOTE1
01302      END-IF.                                                      ELSNOTE1
01303      EVALUATE TRUE                                                ELSNOTE1
01304         WHEN PROCESSING-CBL                                       ELSNOTE1
01305            MOVE WS-TCBL TO TCAR-FROM-LINE(TCAR-TO-SUB)            ELSNOTE1
01306            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
01307         WHEN PROCESSING-CPO                                       ELSNOTE1
01308            MOVE WS-TCPO TO TCAR-FROM-LINE(TCAR-TO-SUB)            ELSNOTE1
01309            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
01310         WHEN PROCESSING-RPO                                       ELSNOTE1
01311            MOVE WS-TRPO TO TCAR-FROM-LINE(TCAR-TO-SUB)            ELSNOTE1
01312            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
01313         WHEN PROCESSING-PPO                                       ELSNOTE1
01314            MOVE WS-TPPO TO TCAR-FROM-LINE(TCAR-TO-SUB)            ELSNOTE1
01315            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
01316         WHEN PROCESSING-POS                                       ELSNOTE1
01317            MOVE WS-TPOS TO TCAR-FROM-LINE(TCAR-TO-SUB)            ELSNOTE1
01318            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
01319         WHEN PROCESSING-PAN                                       ELSNOTE1
01320            MOVE WS-TPAN TO TCAR-FROM-LINE(TCAR-TO-SUB)            ELSNOTE1
01321            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
01322         WHEN PROCESSING-MCN                                       ELSNOTE1
01323            MOVE WS-TMCN TO TCAR-FROM-LINE(TCAR-TO-SUB)            ELSNOTE1
01324            ADD 1 TO TCAR-TO-SUB                                   ELSNOTE1
01325      END-EVALUATE.                                                ELSNOTE1
01326      PERFORM MOVE-RAW-TEXT                                        ELSNOTE1
01327          VARYING WS-MOVE-SUB FROM 1 BY 1                          ELSNOTE1
01328                  UNTIL WS-MOVE-SUB > CMF-NBR-DESCR-LINES          ELSNOTE1
01329                     OR TCAR-TO-SUB = 18.                          ELSNOTE1
01330                                                                   ELSNOTE1
01331 ************************************************************      ELSNOTE1
01332 *                                                          *      ELSNOTE1
01333 *        FORMAT DATES                                      *      ELSNOTE1
01334 *                                                          *      ELSNOTE1
01335 ************************************************************      ELSNOTE1
01336  FORMAT-DATES.                                                    ELSNOTE1
01337      ADD 1 TO TCAR-TO-SUB.                                        ELSNOTE1
01338      MOVE KTG-EFF-DT (KTG-IDX) TO HGADATE-JULIAN1.                ELSNOTE1
01339      PERFORM CONVERT-DATE.                                        ELSNOTE1
01340      MOVE HGADATE-DATE2 TO WS-FROM-DATE.                          ELSNOTE1
01341                                                                   ELSNOTE1
01342      IF KTG-TERM-DT-CENTURY (KTG-IDX) NOT = 9999365               ELSNOTE1
01343          MOVE KTG-TERM-DT-CENTURY (KTG-IDX) TO MLDATE-DATE1       ELSNOTE1
01344          PERFORM CONVERT-CEN-DATE                                 ELSNOTE1
01345          MOVE  MLDATE-DATE2 TO WS-TO-DATE                         ELSNOTE1
01346      ELSE                                                         ELSNOTE1
01347          MOVE WS-YEAR-END-DATE TO WS-TO-DATE                      ELSNOTE1
01348      END-IF.                                                      ELSNOTE1
01349      MOVE WS-DATE TO TCAR-FROM-LINE (TCAR-TO-SUB).                ELSNOTE1
01350                                                                   ELSNOTE1
01351      ADD 1 TO TCAR-TO-SUB.                                        ELSNOTE1
01352                                                                   ELSNOTE1
01353 ************************************************************      ELSNOTE1
01354 *                                                          *      ELSNOTE1
01355 *        CONVERT DATE                                      *      ELSNOTE1
01356 *                                                          *      ELSNOTE1
01357 ************************************************************      ELSNOTE1
01358  CONVERT-DATE.                                                    ELSNOTE1
01359      MOVE 'CNV' TO HGADATE-FUNC.                                  ELSNOTE1
01360      MOVE 'J'   TO HGADATE-FORM1.                                 ELSNOTE1
01361      MOVE 'M'   TO HGADATE-FORM2.                                 ELSNOTE1
01362      MOVE ZERO  TO HGADATE-RETURN                                 ELSNOTE1
01363                    HGADATE-AMOUNT                                 ELSNOTE1
01364                    HGADATE-DATE2.                                 ELSNOTE1
01365      EXEC CICS LINK PROGRAM('HGADATES')                           ELSNOTE1
01366                COMMAREA(HGADATE-PARM)                             ELSNOTE1
01367           END-EXEC.                                               ELSNOTE1
01368                                                                   ELSNOTE1
01369 ************************************************************      ELSNOTE1
01370 *                                                          *      ELSNOTE1
01371 *        CONVERT DATES FROM JULIAN TO GREGORIAN            *      ELSNOTE1
01372 *        IN FORMAT MMDDCCYY                                *      ELSNOTE1
01373 *                                                          *      ELSNOTE1
01374 ************************************************************      ELSNOTE1
01375  CONVERT-CEN-DATE.                                                ELSNOTE1
01376         MOVE 'CNV' TO  MLDATE-FUNC.                               ELSNOTE1
01377         MOVE 'J'   TO  MLDATE-FORM1.                              ELSNOTE1
01378         MOVE 'M'   TO  MLDATE-FORM2.                              ELSNOTE1
01379         MOVE ZEROS TO  MLDATE-RETURN,  MLDATE-AMOUNT.             ELSNOTE1
01380        EXEC CICS LINK PROGRAM ('MLDATEC')                         ELSNOTE1
01381                       COMMAREA (MLDATE01)                         ELSNOTE1
01382                       END-EXEC.                                   ELSNOTE1
01383                                                                   ELSNOTE1
01384                                                                   ELSNOTE1
01385 ************************************************************      ELSNOTE1
01386 *                                                          *      ELSNOTE1
01387 *        MOVE RAW TEXT                                     *      ELSNOTE1
01388 *                                                          *      ELSNOTE1
01389 ************************************************************      ELSNOTE1
01390  MOVE-RAW-TEXT.                                                   ELSNOTE1
01391      ADD 1 TO TCAR-TO-SUB.                                        ELSNOTE1
01392      MOVE CMF-DESCR-LINE (WS-MOVE-SUB)                            ELSNOTE1
01393           TO TCAR-FROM-LINE (TCAR-TO-SUB).                        ELSNOTE1
01394                                                                   ELSNOTE1
01395 ************************************************************      ELSNOTE1
01396 *                                                          *      ELSNOTE1
01397 *        PREPARE TEXT LINES FOR OUTPUT                     *      ELSNOTE1
01398 *                                                          *      ELSNOTE1
01399 ************************************************************      ELSNOTE1
01400  PREPARE-TEXT-LINES.                                              ELSNOTE1
01401      MOVE TCAR-OPF-DATA (TCAR-TO-SUB)                             ELSNOTE1
01402                TO WS-DETAIL-LINE-2A (WS-IDX).                     ELSNOTE1
01403      SET WS-IDX UP BY 1.                                          ELSNOTE1
01404                                                                   ELSNOTE1
01405                                                                   ELSNOTE1
01406 ************************************************************      ELSNOTE1
01407 *                                                          *      ELSNOTE1
01408 *        MOVE TEXT LINES                                   *      ELSNOTE1
01409 *                                                          *      ELSNOTE1
01410 ************************************************************      ELSNOTE1
01411  MOVE-TEXT-LINES.                                                 ELSNOTE1
01412      MOVE WS-DETAIL-LINE-2 (WS-IDX)                               ELSNOTE1
01413                TO MSD-DESCR-LINE (MSD-IDX).                       ELSNOTE1
01414      SET WS-IDX UP BY 1.                                          ELSNOTE1
01415                                                                   ELSNOTE1
01416 ************************************************************      ELSNOTE1
01417      COPY ELSTCOMP.                                               ELSNOTE1
