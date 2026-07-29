00001  IDENTIFICATION DIVISION.                                         12/13/05
00002                                                                   ELXPMCGA
00003  PROGRAM-ID.         ELXPMCGA.                                       LV004
00004                                                                   ELXPMCGA
00005  AUTHOR.             ANNE KEFFER-KING.                            ELXPMCGA
00006                                                                   ELXPMCGA
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELXPMCGA
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELXPMCGA
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELXPMCGA
00010                      233 N. MICHIGAN AVE                          ELXPMCGA
00011                      CHICAGO, ILLINOIS 60601                      ELXPMCGA
00012                                                                   ELXPMCGA
00013  DATE-WRITTEN.       22-JUL-1992.                                 ELXPMCGA
00014                                                                   ELXPMCGA
00015  DATE-COMPILED.                                                   ELXPMCGA
00016                                                                   ELXPMCGA
00017  SECURITY.           COPYRIGHT 1992,                              ELXPMCGA
00018                      HEALTH CARE SERVICE CORPORATION              ELXPMCGA
00019      SKIP3                                                        ELXPMCGA
00020  ENVIRONMENT DIVISION.                                            ELXPMCGA
00021                                                                   ELXPMCGA
00022  CONFIGURATION SECTION.                                           ELXPMCGA
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELXPMCGA
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELXPMCGA
00025      EJECT                                                        ELXPMCGA
00026 ******************************************************************ELXPMCGA
00027 *AKK 12/06/05 REGEN FOR TEST                                     *ELXPMCGA
00028 *   PROGRAM:  ELXPMCGA                                           *ELXPMCGA
00029 *   DATE:     07/22/92                                           *ELXPMCGA
00030 *   AUTHOR:   ANNE KEFFER-KING                                   *ELXPMCGA
00031 *   FUNCTION:                                                    *ELXPMCGA
00032 *       THIS PROGRAM WILL DETERMINE COVERAGE FOR COB, CHC,       *ELXPMCGA
00033 *       PRE-EXISITING CONDITIONS, OB WAITING PERIOD, AND TIMELY  *ELXPMCGA
00034 *       FILING PROVISIONS.                                       *ELXPMCGA
00035 *                                                                *ELXPMCGA
00036 *                                                                *ELXPMCGA
00037 ******************************************************************ELXPMCGA
00038 *                      MAINTENANCE HISTORY                       *ELXPMCGA
00039 *                                                                *ELXPMCGA
00040 *  MOD     DATE     BY  DRPT                ACTION               *ELXPMCGA
00041 * ----- ----------- --- ----- ---------------------------------- *ELXPMCGA
00042 * 01.00 22-JUL-1992 AKK       CREATED                            *ELXPMCGA
00043 *                                                                *ELXPMCGA
00044 * 01.01 16-SEP-1992 AKK       CHANGES COB QUESTION FROM > ZERO   *ELXPMCGA
00045 *                             TO NOT EQUAL ZERO AS WE WERE GET-  *ELXPMCGA
00046 *                             INCORRECT RESULTS.                 *ELXPMCGA
00047 *                                                                *ELXPMCGA
00048 * 01.02 03-MAR-1993 BAK       CHANCE CHC TO READ NO-REQUIREMENTS *ELXPMCGA
00049 *                             INSTEAD OF NO-COVERAGE WHEN PRIOR  *ELXPMCGA
00050 *                             ADMISSION CODE = 0.                *ELXPMCGA
00051 * 02.00 15-JUN-1993 BAK       ADD MAJOR MEDICAL SUPPORT FOR      *ELXPMCGA
00052 *                              CONTRACT ISSR #13071              *ELXPMCGA
00053 * 02.01 19-JUN-1993 AKK       ADD MAJOR MEDICAL SUPPORT FOR      *ELXPMCGA
00054 *                             WAITING-PERIOD.                    *ELXPMCGA
00055 * 02.02 29-JUN-1993 AKK       ADD ADDITONAL SUPPORT FOR WATING   *ELXPMCGA
00056 *                             PERIODS TO SET YES ONLY FOR THOSE  *ELXPMCGA
00057 *                             WATING PERIODS CONTROLLED BY THE   *ELXPMCGA
00058 *                             SUBS EFFECTIVE DATE AND CALL FOR   *ELXPMCGA
00059 *                             OTHERS.  IN ADDITION, TO LOOK AT   *ELXPMCGA
00060 *                             EXPENSE FREE PERIODS WHERE NO WP   *ELXPMCGA
00061 *                             EXIST.                             *ELXPMCGA
00062 * 03.00 20-SEP-1993 BAK       ISSR #13071 PHASE 2 SUPPORT        *ELXPMCGA
00063 *                             ADD WAITING PERIOD DAY FOR PRE-    *ELXPMCGA
00064 *                             EXISTING AND OB ALSO WAIVER IND.   *ELXPMCGA
00065 *                                                                *ELXPMCGA
00066 * 03.01 29-SEPTEMBER 1993 RGO-UPDATE WS-PAT-WP     TABLE TO      *ELXPMCGA
00067 *                             HANDLE NEW BC AND BS WAITG-PERD-IND*ELXPMCGA
00068 *                             VALUES. MODIFIED PARAGRAPH 4500-   *ELXPMCGA
00069 *                             TO DEFAULT TO CALL.                *ELXPMCGA
00070 *                            -FIXED LOGIC BUGS IN 4100-,4200- AND*ELXPMCGA
00071 *                             4300-. ADDED \
00072 *                             CODE FOR PMCI-SPOUSE AND           *ELXPMCGA
00073 *                             DEPENDENT WAS NEVER GETTING        *ELXPMCGA
00074 *                             EXECUTED.                          *ELXPMCGA
00075 *                                                                 ELXPMCGA
00076 * 03.02 01-APRIL-2003     AKK MORE ENDEVOR CHANGES               *ELXPMCGA
00077 * 04.00 02-DEC-2003 AKK RECOMPILE FOR COPYBOOK CHANGES           *ELXPMCGA
00078 *                                                                *ELXPMCGA
00079 * 04.01 09-JAN-2004 AKK S0C7 INTERTEST                           *ELXPMCGA
00080 *                                                                *ELXPMCGA
00081 * 05.02 23-JAN-2004 AKK RECOMPILE AFTER COMPILER FIX             *ELXPMCGA
00082 ******************************************************************ELXPMCGA
00083                                                                   ELXPMCGA
00084 /                                                                 ELXPMCGA
00085  DATA DIVISION.                                                   ELXPMCGA
00086  WORKING-STORAGE SECTION.                                         ELXPMCGA
00087  01  WS-HDR                 PIC X(42) VALUE                       ELXPMCGA
00088      '***ELXPMCGA WORKING STORAGE BEGINS HERE***'.                ELXPMCGA
00089                                                                   ELXPMCGA
00090  01  WS-SWITCHES.                                                 ELXPMCGA
00091      02                          PICTURE  X(01).                  ELXPMCGA
00092         88 SW-NO-TRMNL-ERR       VALUE 'N'.                       ELXPMCGA
00093         88 SW-TRMNL-ERR          VALUE 'Y'.                       ELXPMCGA
00094                                                                   ELXPMCGA
00095  01  WS-GCT-LOB             PIC X(01) VALUE ZEROS.                ELXPMCGA
00096                                                                   ELXPMCGA
00097  01  WS-COB-CODES.                                                ELXPMCGA
00098      05  WS-COB-CODE        PIC X(01).                            ELXPMCGA
00099          88 WS-GENDER-APPLIES         VALUE '1' '2' '3'           ELXPMCGA
00100                                             '4' '5' '6'           ELXPMCGA
00101                                             '7'.                  ELXPMCGA
00102          88 WS-BIRTHDAY-APPLIES       VALUE 'F' 'G' 'H'           ELXPMCGA
00103                                             'I' 'J' 'K'           ELXPMCGA
00104                                             'L'.                  ELXPMCGA
00105 * ADDED 0Y AND 10 VALUES. RGO 9/94                               *ELXPMCGA
00106  01  WS-WATING-PERIOD-CODES.                                      ELXPMCGA
00107      05  WS-HOLD-WP-IND               PIC X(02).                  ELXPMCGA
00108          88  WS-PAT-WP                    VALUE '0C' '0D' '0E'    ELXPMCGA
00109                                                 '0H' '0I' '0J'    ELXPMCGA
00110                                                 '0K' '0M' '0N'    ELXPMCGA
00111                                                 '0P' '0Q' '0S'    ELXPMCGA
00112                                                 '0T' '0U' '0V'    ELXPMCGA
00113                                                 '0W' '01' '02'    ELXPMCGA
00114                                                 '03' '04' '05'    ELXPMCGA
00115                                                 '0Y' '10'.        ELXPMCGA
00116          88  WS-SBSCRBR-WP                VALUE '0A' '0F' '06'    ELXPMCGA
00117                                                 '07' '08' '09'.   ELXPMCGA
00118          88  WS-OTHR-WP                   VALUE '0B' '0G' '0L'    ELXPMCGA
00119                                                 '0R'.             ELXPMCGA
00120                                                                   ELXPMCGA
00121      05  WS-WAIVER-IND                PIC X(02).                  ELXPMCGA
00122          88  WS-WAIVE-NONE                VALUE '00'.             ELXPMCGA
00123          88  WS-WAIVE-INI-ENREE           VALUE '0D' '0F' '0R'.   ELXPMCGA
00124          88  WS-WAIVE-INI-ENRMT           VALUE '01'.             ELXPMCGA
00125          88  WS-WAIVE-CALL                VALUE '0A' '0B' '0C'    ELXPMCGA
00126                                                 '0E' '0G' '0H'    ELXPMCGA
00127                                                 '0I' '0J' '0K'    ELXPMCGA
00128                                                 '0L' '0M' '0N'    ELXPMCGA
00129                                                 '0P' '0Q' '0S'    ELXPMCGA
00130                                                 '02' '03' '04'    ELXPMCGA
00131                                                 '05' '06' '07'    ELXPMCGA
00132                                                 '08' '09'.        ELXPMCGA
00133                                                                   ELXPMCGA
00134                                                                   ELXPMCGA
00135      05  WS-OB-WAIVER-IND                PIC X(02).               ELXPMCGA
00136          88  WS-OB-WAIVE-NONE             VALUE '00'.             ELXPMCGA
00137          88  WS-OB-WAIVE-INI-ENREE        VALUE '0F' '0R'.        ELXPMCGA
00138          88  WS-OB-WAIVE-INI-ENRMT        VALUE '01'.             ELXPMCGA
00139          88  WS-OB-WAIVE-CALL             VALUE '0A' '0B' '0C'    ELXPMCGA
00140                                                 '0D' '0E' '0G'    ELXPMCGA
00141                                                 '0H' '0I' '0J'    ELXPMCGA
00142                                                 '0K' '0L' '0M'    ELXPMCGA
00143                                                 '0N' '0P' '0Q'    ELXPMCGA
00144                                                 '0R' '0S' '01'    ELXPMCGA
00145                                                 '02' '03' '04'    ELXPMCGA
00146                                                 '05' '06' '07'    ELXPMCGA
00147                                                 '08' '09'.        ELXPMCGA
00148                                                                   ELXPMCGA
00149      05  WS-CHC-D-H-CODE    PIC X(01).                            ELXPMCGA
00150          88 WS-24-HOURS               VALUE '1' '6' 'A'.          ELXPMCGA
00151          88 WS-72-HOURS               VALUE '2' '5'.              ELXPMCGA
00152          88 WS-7-DAYS                 VALUE '3' '7'.              ELXPMCGA
00153          88 WS-14-DAYS                VALUE '4' '8'.              ELXPMCGA
00154          88 WS-10-DAYS                VALUE 'B'.                  ELXPMCGA
00155          88 WS-30-DAYS                VALUE 'E'.                  ELXPMCGA
00156          88 WS-X-CODE                 VALUE 'X'.                  ELXPMCGA
00157          88 WS-0-CODE                 VALUE '0'.                  ELXPMCGA
00158                                                                   ELXPMCGA
00159                                                                   ELXPMCGA
00160      05  WS-TIMELY-FILING   PIC X(02).                            ELXPMCGA
00161          88 WS-NO-LIMIT               VALUE '00'.                 ELXPMCGA
00162          88 WS-2-MONTHS               VALUE '0E'.                 ELXPMCGA
00163          88 WS-6-MONTHS               VALUE '0F'.                 ELXPMCGA
00164          88 WS-15-MONTHS              VALUE '0I'.                 ELXPMCGA
00165          88 WS-12-MONTHS              VALUE 'OK' '03'.            ELXPMCGA
00166          88 WS-18-MONTHS              VALUE '04'.                 ELXPMCGA
00167          88 WS-24-MONTHS              VALUE '05'.                 ELXPMCGA
00168          88 WS-30-MONTHS              VALUE '06'.                 ELXPMCGA
00169          88 WS-36-MONTHS              VALUE '07'.                 ELXPMCGA
00170          88 WS-48-MONTHS              VALUE '08'.                 ELXPMCGA
00171          88 WS-60-MONTHS              VALUE '09'.                 ELXPMCGA
00172                                                                   ELXPMCGA
00173      05  WS-0               PIC S9(03) COMP-3                     ELXPMCGA
00174                                       VALUE 0.                    ELXPMCGA
00175      05  WS-02              PIC S9(03) COMP-3                     ELXPMCGA
00176                                       VALUE 2.                    ELXPMCGA
00177      05  WS-06              PIC S9(03) COMP-3                     ELXPMCGA
00178                                       VALUE 6.                    ELXPMCGA
00179      05  WS-07              PIC S9(03) COMP-3                     ELXPMCGA
00180                                       VALUE 07.                   ELXPMCGA
00181      05  WS-10              PIC S9(03) COMP-3                     ELXPMCGA
00182                                       VALUE 10.                   ELXPMCGA
00183      05  WS-12              PIC S9(03) COMP-3                     ELXPMCGA
00184                                       VALUE 12.                   ELXPMCGA
00185      05  WS-14              PIC S9(03) COMP-3                     ELXPMCGA
00186                                       VALUE 14.                   ELXPMCGA
00187      05  WS-15              PIC S9(03) COMP-3                     ELXPMCGA
00188                                       VALUE 15.                   ELXPMCGA
00189      05  WS-18              PIC S9(03) COMP-3                     ELXPMCGA
00190                                       VALUE 18.                   ELXPMCGA
00191      05  WS-24              PIC S9(03) COMP-3                     ELXPMCGA
00192                                       VALUE 24.                   ELXPMCGA
00193      05  WS-30              PIC S9(03) COMP-3                     ELXPMCGA
00194                                       VALUE 30.                   ELXPMCGA
00195      05  WS-36              PIC S9(03) COMP-3                     ELXPMCGA
00196                                       VALUE 36.                   ELXPMCGA
00197      05  WS-48              PIC S9(03) COMP-3                     ELXPMCGA
00198                                       VALUE 48.                   ELXPMCGA
00199      05  WS-60              PIC S9(03) COMP-3                     ELXPMCGA
00200                                       VALUE 60.                   ELXPMCGA
00201      05  WS-72              PIC S9(03) COMP-3                     ELXPMCGA
00202                                       VALUE 72.                   ELXPMCGA
00203      05  WS-X               PIC X(01) VALUE 'X'.                  ELXPMCGA
00204 /                                                                 ELXPMCGA
00205  LINKAGE SECTION.                                                 ELXPMCGA
00206  01  DFHCOMMAREA.                                                 ELXPMCGA
00207      COPY ELSCOMMC.                                               ELXPMCGA
00208 /                                                                 ELXPMCGA
00209      COPY ELSCIA2C.                                               ELXPMCGA
00210 /                                                                 ELXPMCGA
00211      COPY ELSPMCID.                                               ELXPMCGA
00212 /                                                                 ELXPMCGA
00213  01  PMCI-COMM-AREA.                                              ELXPMCGA
00214      COPY PMCCOMM.                                                ELXPMCGA
00215 /                                                                 ELXPMCGA
00216  01  GROUP-RECORD.                                                ELXPMCGA
00217      COPY GCGROUPC.                                               ELXPMCGA
00218 /                                                                 ELXPMCGA
00219  01  CONTRACT-RECORD.                                             ELXPMCGA
00220      COPY GCCONTRC.                                               ELXPMCGA
00221 /                                                                 ELXPMCGA
00222      EJECT                                                        ELXPMCGA
00223 ************************************************************      ELXPMCGA
00224 *                                                          *      ELXPMCGA
00225 *                    PROCEDURE DIVISION                    *      ELXPMCGA
00226 *                                                          *      ELXPMCGA
00227 ************************************************************      ELXPMCGA
00228                                                                   ELXPMCGA
00229  PROCEDURE DIVISION.                                              ELXPMCGA
00230                                                                   ELXPMCGA
00231 ************************************************************      ELXPMCGA
00232 *                                                          *      ELXPMCGA
00233 *    GENERAL ADMINISTRATION RULES                          *      ELXPMCGA
00234 *    FOR NOTICE OF ADMISSION/ELIGIBILITY SUMMARY           *      ELXPMCGA
00235 *                                                          *      ELXPMCGA
00236 ************************************************************      ELXPMCGA
00237                                                                   ELXPMCGA
00238  0000-GRP-CONTRACT-MAINLINE.                                      ELXPMCGA
00239      SET SW-NO-TRMNL-ERR TO TRUE.                                 ELXPMCGA
00240      IF ECA-CIA-PTR = NULL                                        ELXPMCGA
00241         CONTINUE                                                  ELXPMCGA
00242      ELSE                                                         ELXPMCGA
00243         PERFORM 0100-ESTABLISH-ADDRESSABILITY                     ELXPMCGA
00244         IF SW-NO-TRMNL-ERR                                        ELXPMCGA
00245            PERFORM 0500-EXTRACT-GRP-CNTRCT-ELMNTS                 ELXPMCGA
00246         ELSE                                                      ELXPMCGA
00247            CONTINUE                                               ELXPMCGA
00248         END-IF                                                    ELXPMCGA
00249      END-IF.                                                      ELXPMCGA
00250      GOBACK.                                                      ELXPMCGA
00251 /                                                                 ELXPMCGA
00252  0100-ESTABLISH-ADDRESSABILITY.                                   ELXPMCGA
00253      CALL 'ELUINISM' USING DFHCOMMAREA                            ELXPMCGA
00254                     ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.     ELXPMCGA
00255      SET CIA-PMCCOMM-DDN TO TRUE.                                 ELXPMCGA
00256      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCGA
00257                     ADDRESS OF PMCI-COMM-AREA.                    ELXPMCGA
00258      IF CIA-RC-OK                                                 ELXPMCGA
00259         SET PMCI-BC-SUCCESSFUL TO TRUE                            ELXPMCGA
00260         SET PMCI-BC-NO-ERROR TO TRUE                              ELXPMCGA
00261         SET CIA-ELSGRPSP-DDN TO TRUE                              ELXPMCGA
00262         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCGA
00263                              ADDRESS OF GROUP-RECORD              ELXPMCGA
00264         IF CIA-RC-OK                                              ELXPMCGA
00265            PERFORM 0200-TEST-CONTRACT-ADDRESS                     ELXPMCGA
00266            IF CIA-RC-OK                                           ELXPMCGA
00267               SET CIA-ELSPMCID-DDN TO TRUE                        ELXPMCGA
00268               CALL 'ELUSETAD' USING DFHCOMMAREA                   ELXPMCGA
00269                              ADDRESS OF NAES-INTERMEDIATE-DATA    ELXPMCGA
00270               IF CIA-RC-OK                                        ELXPMCGA
00271                  CONTINUE                                         ELXPMCGA
00272               ELSE                                                ELXPMCGA
00273                  SET SW-TRMNL-ERR TO TRUE                         ELXPMCGA
00274                  SET PMCI-BC-INTERNAL-ERROR TO TRUE               ELXPMCGA
00275                  MOVE +2003 TO PMCI-BLUE-CHIP-ERROR-CODE          ELXPMCGA
00276               END-IF                                              ELXPMCGA
00277            ELSE                                                   ELXPMCGA
00278               SET SW-TRMNL-ERR TO TRUE                            ELXPMCGA
00279               SET PMCI-BC-INTERNAL-ERROR TO TRUE                  ELXPMCGA
00280               MOVE +2002 TO PMCI-BLUE-CHIP-ERROR-CODE             ELXPMCGA
00281            END-IF                                                 ELXPMCGA
00282         ELSE                                                      ELXPMCGA
00283            SET SW-TRMNL-ERR TO TRUE                               ELXPMCGA
00284            SET PMCI-BC-INTERNAL-ERROR TO TRUE                     ELXPMCGA
00285            MOVE +2001 TO PMCI-BLUE-CHIP-ERROR-CODE                ELXPMCGA
00286         END-IF                                                    ELXPMCGA
00287      ELSE                                                         ELXPMCGA
00288         SET SW-TRMNL-ERR TO TRUE                                  ELXPMCGA
00289      END-IF.                                                      ELXPMCGA
00290 /                                                                 ELXPMCGA
00291  0200-TEST-CONTRACT-ADDRESS.                                      ELXPMCGA
00292      SET CIA-ELSCONIB-DDN TO TRUE                                 ELXPMCGA
00293      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELXPMCGA
00294                      ADDRESS OF CONTRACT-RECORD                   ELXPMCGA
00295      IF CIA-RC-OK                                                 ELXPMCGA
00296         CONTINUE                                                  ELXPMCGA
00297      ELSE                                                         ELXPMCGA
00298         SET CIA-ELSCONIS-DDN TO TRUE                              ELXPMCGA
00299         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELXPMCGA
00300                         ADDRESS OF CONTRACT-RECORD.               ELXPMCGA
00301 /                                                                 ELXPMCGA
00302  0500-EXTRACT-GRP-CNTRCT-ELMNTS.                                  ELXPMCGA
00303      PERFORM 1000-DETERMINE-COB-COVERAGE.                         ELXPMCGA
00304      PERFORM 2000-DETERMINE-CHC.                                  ELXPMCGA
00305      PERFORM 4000-DETERMINE-PRE-EXISTING.                         ELXPMCGA
00306      PERFORM 4700-DETERMINE-OB-WAIT-PER.                          ELXPMCGA
00307      PERFORM 8000-DETERMINE-TIMELY-FILING.                        ELXPMCGA
00308 /                                                                 ELXPMCGA
00309  1000-DETERMINE-COB-COVERAGE.                                     ELXPMCGA
00310      EVALUATE TRUE                                                ELXPMCGA
00311      WHEN PMCI-MEMBER                                             ELXPMCGA
00312         IF GCT-COB-MEM-CD NOT EQUAL ZERO                          ELXPMCGA
00313            SET COB-APPLIES TO TRUE                                ELXPMCGA
00314         ELSE                                                      ELXPMCGA
00315            SET COB-NO-CLAUSE TO TRUE                              ELXPMCGA
00316         END-IF                                                    ELXPMCGA
00317      WHEN PMCI-SPOUSE                                             ELXPMCGA
00318         IF GCT-COB-SPS-CD NOT EQUAL ZERO                          ELXPMCGA
00319            MOVE GCT-COB-SPS-CD TO WS-COB-CODE                     ELXPMCGA
00320            PERFORM 1100-DETERMINE-COB-RULE                        ELXPMCGA
00321         ELSE                                                      ELXPMCGA
00322            SET COB-NO-CLAUSE TO TRUE                              ELXPMCGA
00323         END-IF                                                    ELXPMCGA
00324      WHEN PMCI-DEPENDENT                                          ELXPMCGA
00325         IF GCT-COB-DEP-CD NOT EQUAL ZERO                          ELXPMCGA
00326            MOVE GCT-COB-DEP-CD TO WS-COB-CODE                     ELXPMCGA
00327            PERFORM 1100-DETERMINE-COB-RULE                        ELXPMCGA
00328         ELSE                                                      ELXPMCGA
00329            SET COB-NO-CLAUSE TO TRUE                              ELXPMCGA
00330         END-IF                                                    ELXPMCGA
00331      WHEN OTHER                                                   ELXPMCGA
00332         SET PMCI-BC-INVALID-DATA TO TRUE                          ELXPMCGA
00333         MOVE +2004 TO PMCI-BLUE-CHIP-ERROR-CODE                   ELXPMCGA
00334      END-EVALUATE.                                                ELXPMCGA
00335                                                                   ELXPMCGA
00336  1100-DETERMINE-COB-RULE.                                         ELXPMCGA
00337      IF WS-GENDER-APPLIES                                         ELXPMCGA
00338         SET COB-GENDER-RULE TO TRUE                               ELXPMCGA
00339      ELSE                                                         ELXPMCGA
00340         IF WS-BIRTHDAY-APPLIES                                    ELXPMCGA
00341            SET COB-BIRTHDAY-RULE TO TRUE                          ELXPMCGA
00342         ELSE                                                      ELXPMCGA
00343            SET COB-CALL TO TRUE                                   ELXPMCGA
00344         END-IF                                                    ELXPMCGA
00345      END-IF.                                                      ELXPMCGA
00346                                                                   ELXPMCGA
00347  2000-DETERMINE-CHC.                                              ELXPMCGA
00348      MOVE GCG-CHC-PRIOR-ADMISSION TO WS-CHC-D-H-CODE.             ELXPMCGA
00349      EVALUATE TRUE                                                ELXPMCGA
00350         WHEN WS-24-HOURS                                          ELXPMCGA
00351            MOVE WS-24 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00352            SET CHC-HOURS-REQUIREMENT TO TRUE                      ELXPMCGA
00353         WHEN WS-72-HOURS                                          ELXPMCGA
00354            MOVE WS-72 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00355            SET CHC-HOURS-REQUIREMENT TO TRUE                      ELXPMCGA
00356         WHEN WS-7-DAYS                                            ELXPMCGA
00357            MOVE WS-07 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00358            SET CHC-DAYS-REQUIREMENT TO TRUE                       ELXPMCGA
00359         WHEN WS-14-DAYS                                           ELXPMCGA
00360            MOVE WS-14 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00361            SET CHC-DAYS-REQUIREMENT TO TRUE                       ELXPMCGA
00362         WHEN WS-10-DAYS                                           ELXPMCGA
00363            MOVE WS-10 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00364            SET CHC-DAYS-REQUIREMENT TO TRUE                       ELXPMCGA
00365         WHEN WS-30-DAYS                                           ELXPMCGA
00366            MOVE WS-30 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00367            SET CHC-DAYS-REQUIREMENT TO TRUE                       ELXPMCGA
00368         WHEN WS-X-CODE                                            ELXPMCGA
00369            MOVE WS-X TO PMCI-CHC-REQUIREMENTS                     ELXPMCGA
00370            SET CHC-UNLIMITED TO TRUE                              ELXPMCGA
00371         WHEN WS-0-CODE                                            ELXPMCGA
00372            MOVE WS-0 TO PMCI-CHC-REQUIREMENTS                     ELXPMCGA
00373            SET CHC-NO-REQUIREMENTS TO TRUE                        ELXPMCGA
00374         WHEN OTHER                                                ELXPMCGA
00375            SET CHC-CALL TO TRUE                                   ELXPMCGA
00376         END-EVALUATE.                                             ELXPMCGA
00377 ****************************************************************  ELXPMCGA
00378 *                                                                 ELXPMCGA
00379 *  DETERMINE IF PRE-EXISTING CONDITIONS EXISTS                    ELXPMCGA
00380 *                                                                 ELXPMCGA
00381 ****************************************************************  ELXPMCGA
00382  4000-DETERMINE-PRE-EXISTING.                                     ELXPMCGA
00383      SET PMCI-PRE-WAVE-NONE TO TRUE.                              ELXPMCGA
00384      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES AND                  ELXPMCGA
00385                  PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES              ELXPMCGA
00386         PERFORM 4010-DETERMINE-BOTH                               ELXPMCGA
00387      ELSE                                                         ELXPMCGA
00388      IF PMCI-MM-CNTRCT-GRP = SPACES                               ELXPMCGA
00389         PERFORM 4020-DETERMINE-BASIC                              ELXPMCGA
00390      ELSE                                                         ELXPMCGA
00391      IF PMCI-BSC-CNTRCT-GRP = SPACES                              ELXPMCGA
00392         PERFORM 4030-DETERMINE-MAJ-MED.                           ELXPMCGA
00393 ****************************************************************  ELXPMCGA
00394 *                                                                 ELXPMCGA
00395 *  DETERMINE IF PRE-EXISTING CONDITIONS EXISTS FOR BOTH           ELXPMCGA
00396 *                                                                 ELXPMCGA
00397 ****************************************************************  ELXPMCGA
00398  4010-DETERMINE-BOTH.                                             ELXPMCGA
00399      IF PMCI-INSTITUTIONAL                                        ELXPMCGA
00400         IF GCG-BC-WAITG-PERD-IND NOT EQUAL '00' OR                ELXPMCGA
00401                       GCG-MM-WAITG-PERD-IND NOT EQUAL '00'        ELXPMCGA
00402            PERFORM 4050-EVALUATE-BC-BOTH-PREEX                    ELXPMCGA
00403         ELSE                                                      ELXPMCGA
00404            SET PMCI-PRE-EXIST-NOT-APPL TO TRUE                    ELXPMCGA
00405      ELSE                                                         ELXPMCGA
00406      IF PMCI-PROFESSIONAL                                         ELXPMCGA
00407         IF GCG-BS-WAITG-PERD-IND NOT EQUAL '00' OR                ELXPMCGA
00408                        GCG-MM-WAITG-PERD-IND NOT EQUAL '00'       ELXPMCGA
00409            PERFORM 4060-EVALUATE-BS-BOTH-PREEX                    ELXPMCGA
00410         ELSE                                                      ELXPMCGA
00411           SET PMCI-PRE-EXIST-NOT-APPL TO TRUE.                    ELXPMCGA
00412 ****************************************************************  ELXPMCGA
00413 *                                                                 ELXPMCGA
00414 *  DETERMINE IF PRE-EXISTING CONDITIONS EXISTS FOR BASIC          ELXPMCGA
00415 *                                                                 ELXPMCGA
00416 ****************************************************************  ELXPMCGA
00417  4020-DETERMINE-BASIC.                                            ELXPMCGA
00418      IF PMCI-INSTITUTIONAL                                        ELXPMCGA
00419         IF GCG-BC-WAITG-PERD-IND NOT EQUAL '00'                   ELXPMCGA
00420            SET PMCI-PRE-BSC TO TRUE                               ELXPMCGA
00421            PERFORM 4100-EVALUATE-BC-PREEX                         ELXPMCGA
00422         ELSE                                                      ELXPMCGA
00423            SET PMCI-PRE-EXIST-NOT-APPL TO TRUE                    ELXPMCGA
00424      ELSE                                                         ELXPMCGA
00425      IF PMCI-PROFESSIONAL                                         ELXPMCGA
00426         IF GCG-BS-WAITG-PERD-IND NOT EQUAL '00'                   ELXPMCGA
00427            SET PMCI-PRE-BSC TO TRUE                               ELXPMCGA
00428            PERFORM 4200-EVALUATE-BS-PREEX                         ELXPMCGA
00429         ELSE                                                      ELXPMCGA
00430            SET PMCI-PRE-EXIST-NOT-APPL TO TRUE.                   ELXPMCGA
00431 ****************************************************************  ELXPMCGA
00432 *                                                                 ELXPMCGA
00433 *  DETERMINE IF PRE-EXISTING CONDITIONS EXISTS FOR MAJOR MED      ELXPMCGA
00434 *                                                                 ELXPMCGA
00435 ****************************************************************  ELXPMCGA
00436  4030-DETERMINE-MAJ-MED.                                          ELXPMCGA
00437      IF GCG-MM-WAITG-PERD-IND NOT EQUAL '00'                      ELXPMCGA
00438         SET PMCI-PRE-MM TO TRUE                                   ELXPMCGA
00439         PERFORM 4300-EVALUATE-MM-PREEX                            ELXPMCGA
00440      ELSE                                                         ELXPMCGA
00441         SET PMCI-PRE-EXIST-NOT-APPL TO TRUE.                      ELXPMCGA
00442 ****************************************************************  ELXPMCGA
00443 *                                                                 ELXPMCGA
00444 *  EVALUATE WAITING PERIOD DAYS FOR BLUE CROSS OR MAJOR MEDICAL   ELXPMCGA
00445 *                                                                 ELXPMCGA
00446 ****************************************************************  ELXPMCGA
00447  4050-EVALUATE-BC-BOTH-PREEX.                                     ELXPMCGA
00448         IF GCG-BC-WAITG-PERD-MEM-DAYS NOT EQUAL ZERO AND          ELXPMCGA
00449                      GCG-MM-WAITG-PERD-MEM-DAYS NOT EQUAL ZERO    ELXPMCGA
00450            SET PMCI-PRE-BSC-MM TO TRUE                            ELXPMCGA
00451            PERFORM 4100-EVALUATE-BC-PREEX                         ELXPMCGA
00452      ELSE                                                         ELXPMCGA
00453         IF GCG-MM-WAITG-PERD-MEM-DAYS NOT EQUAL ZERO              ELXPMCGA
00454            SET PMCI-PRE-MM TO TRUE                                ELXPMCGA
00455            PERFORM 4300-EVALUATE-MM-PREEX                         ELXPMCGA
00456      ELSE                                                         ELXPMCGA
00457         SET PMCI-PRE-BSC TO TRUE                                  ELXPMCGA
00458         PERFORM 4100-EVALUATE-BC-PREEX.                           ELXPMCGA
00459 ****************************************************************  ELXPMCGA
00460 *                                                                 ELXPMCGA
00461 *  EVALUATE WAITING PERIOD DAYS FOR BLUE SHIELD OR MAJOR MEDICAL  ELXPMCGA
00462 *                                                                 ELXPMCGA
00463 ****************************************************************  ELXPMCGA
00464  4060-EVALUATE-BS-BOTH-PREEX.                                     ELXPMCGA
00465         IF GCG-BS-WAITG-PERD-MEM-DAYS NOT EQUAL ZERO AND          ELXPMCGA
00466                    GCG-MM-WAITG-PERD-MEM-DAYS NOT EQUAL ZERO      ELXPMCGA
00467            SET PMCI-PRE-BSC-MM TO TRUE                            ELXPMCGA
00468            PERFORM 4200-EVALUATE-BS-PREEX                         ELXPMCGA
00469      ELSE                                                         ELXPMCGA
00470         IF GCG-MM-WAITG-PERD-MEM-DAYS > ZERO                      ELXPMCGA
00471            SET PMCI-PRE-MM TO TRUE                                ELXPMCGA
00472            PERFORM 4300-EVALUATE-MM-PREEX                         ELXPMCGA
00473      ELSE                                                         ELXPMCGA
00474         SET PMCI-PRE-BSC TO TRUE                                  ELXPMCGA
00475         PERFORM 4200-EVALUATE-BS-PREEX.                           ELXPMCGA
00476 ****************************************************************  ELXPMCGA
00477 *                                                                 ELXPMCGA
00478 *  EVALUATE WAITING PERIOD DAYS FOR BLUE CROSS                    ELXPMCGA
00479 *  10/3/94. ADDED END-IF'S. RGO                                   ELXPMCGA
00480 *                                                                 ELXPMCGA
00481 ****************************************************************  ELXPMCGA
00482  4100-EVALUATE-BC-PREEX.                                          ELXPMCGA
00483      MOVE GCG-BC-WAITG-PERD-IND TO WS-HOLD-WP-IND.                ELXPMCGA
00484      MOVE GCG-BC-WAIVR-IND TO WS-WAIVER-IND.                      ELXPMCGA
00485      IF PMCI-MEMBER                                               ELXPMCGA
00486         IF GCG-BC-WAITG-PERD-MEM-DAYS > ZERO                      ELXPMCGA
00487            MOVE GCG-BC-WAITG-PERD-MEM-DAYS TO                     ELXPMCGA
00488                         PMCI-PRE-EXISTING-WAIT-PRD                ELXPMCGA
00489            SET PMCI-PRE-EXIST-APPL TO TRUE                        ELXPMCGA
00490         END-IF                                                    ELXPMCGA
00491      ELSE                                                         ELXPMCGA
00492      IF PMCI-SPOUSE                                               ELXPMCGA
00493         IF GCG-BC-WAITG-PERD-SPS-DAYS > ZERO                      ELXPMCGA
00494            MOVE GCG-BC-WAITG-PERD-SPS-DAYS TO                     ELXPMCGA
00495                         PMCI-PRE-EXISTING-WAIT-PRD                ELXPMCGA
00496            PERFORM 4500-DTRMN-SPS-DEP-WP                          ELXPMCGA
00497         END-IF                                                    ELXPMCGA
00498      ELSE                                                         ELXPMCGA
00499      IF PMCI-DEPENDENT                                            ELXPMCGA
00500         IF GCG-BC-WAITG-PERD-DEP-DAYS > ZERO                      ELXPMCGA
00501            MOVE GCG-BC-WAITG-PERD-DEP-DAYS TO                     ELXPMCGA
00502                         PMCI-PRE-EXISTING-WAIT-PRD                ELXPMCGA
00503            PERFORM 4500-DTRMN-SPS-DEP-WP                          ELXPMCGA
00504         END-IF                                                    ELXPMCGA
00505      END-IF.                                                      ELXPMCGA
00506      PERFORM 4600-DETERMINE-WAIVER.                               ELXPMCGA
00507 ****************************************************************  ELXPMCGA
00508 *                                                                 ELXPMCGA
00509 *  EVALUATE WAITING PERIOD DAYS FOR BLUE SHIELD                   ELXPMCGA
00510 *                                                                 ELXPMCGA
00511 *  10/3/94. ADDED END-IF'S. RGO                                   ELXPMCGA
00512 ****************************************************************  ELXPMCGA
00513  4200-EVALUATE-BS-PREEX.                                          ELXPMCGA
00514      MOVE GCG-BS-WAITG-PERD-IND TO WS-HOLD-WP-IND.                ELXPMCGA
00515      MOVE GCG-BS-WAIVR-IND TO WS-WAIVER-IND.                      ELXPMCGA
00516      IF PMCI-MEMBER                                               ELXPMCGA
00517         IF GCG-BS-WAITG-PERD-MEM-DAYS > ZERO                      ELXPMCGA
00518            MOVE GCG-BS-WAITG-PERD-MEM-DAYS TO                     ELXPMCGA
00519                         PMCI-PRE-EXISTING-WAIT-PRD                ELXPMCGA
00520            SET PMCI-PRE-EXIST-APPL TO TRUE                        ELXPMCGA
00521         END-IF                                                    ELXPMCGA
00522      ELSE                                                         ELXPMCGA
00523      IF PMCI-SPOUSE                                               ELXPMCGA
00524         IF GCG-BS-WAITG-PERD-SPS-DAYS > ZERO                      ELXPMCGA
00525            MOVE GCG-BS-WAITG-PERD-SPS-DAYS TO                     ELXPMCGA
00526                         PMCI-PRE-EXISTING-WAIT-PRD                ELXPMCGA
00527            PERFORM 4500-DTRMN-SPS-DEP-WP                          ELXPMCGA
00528         END-IF                                                    ELXPMCGA
00529      ELSE                                                         ELXPMCGA
00530      IF PMCI-DEPENDENT                                            ELXPMCGA
00531         IF GCG-BS-WAITG-PERD-DEP-DAYS > ZERO                      ELXPMCGA
00532            MOVE GCG-BS-WAITG-PERD-DEP-DAYS TO                     ELXPMCGA
00533                         PMCI-PRE-EXISTING-WAIT-PRD                ELXPMCGA
00534            PERFORM 4500-DTRMN-SPS-DEP-WP                          ELXPMCGA
00535         END-IF                                                    ELXPMCGA
00536       END-IF.                                                     ELXPMCGA
00537                                                                   ELXPMCGA
00538      PERFORM 4600-DETERMINE-WAIVER.                               ELXPMCGA
00539 ****************************************************************  ELXPMCGA
00540 *                                                                 ELXPMCGA
00541 *  EVALUATE WAITING PERIOD DAYS FOR MAJOR MEDICAL                 ELXPMCGA
00542 *                                                                 ELXPMCGA
00543 *  10/3/94. ADDED END-IF'S. RGO                                   ELXPMCGA
00544 ****************************************************************  ELXPMCGA
00545  4300-EVALUATE-MM-PREEX.                                          ELXPMCGA
00546      MOVE GCG-MM-WAITG-PERD-IND TO WS-HOLD-WP-IND.                ELXPMCGA
00547      MOVE GCG-MM-WAIVR-IND TO WS-WAIVER-IND.                      ELXPMCGA
00548      IF PMCI-MEMBER                                               ELXPMCGA
00549         IF GCG-MM-WAITG-PERD-MEM-DAYS > ZERO                      ELXPMCGA
00550            MOVE GCG-MM-WAITG-PERD-MEM-DAYS TO                     ELXPMCGA
00551                         PMCI-PRE-EXISTING-WAIT-PRD                ELXPMCGA
00552            SET PMCI-PRE-EXIST-APPL TO TRUE                        ELXPMCGA
00553         END-IF                                                    ELXPMCGA
00554      ELSE                                                         ELXPMCGA
00555      IF PMCI-SPOUSE                                               ELXPMCGA
00556         IF GCG-MM-WAITG-PERD-SPS-DAYS > ZERO                      ELXPMCGA
00557            MOVE GCG-MM-WAITG-PERD-SPS-DAYS TO                     ELXPMCGA
00558                         PMCI-PRE-EXISTING-WAIT-PRD                ELXPMCGA
00559            PERFORM 4500-DTRMN-SPS-DEP-WP                          ELXPMCGA
00560         END-IF                                                    ELXPMCGA
00561      ELSE                                                         ELXPMCGA
00562      IF PMCI-DEPENDENT                                            ELXPMCGA
00563         IF GCG-BS-WAITG-PERD-DEP-DAYS > ZERO                      ELXPMCGA
00564            MOVE GCG-MM-WAITG-PERD-DEP-DAYS TO                     ELXPMCGA
00565                         PMCI-PRE-EXISTING-WAIT-PRD                ELXPMCGA
00566            PERFORM 4500-DTRMN-SPS-DEP-WP                          ELXPMCGA
00567         END-IF                                                    ELXPMCGA
00568      END-IF.                                                      ELXPMCGA
00569                                                                   ELXPMCGA
00570      PERFORM 4600-DETERMINE-WAIVER.                               ELXPMCGA
00571 ****************************************************************  ELXPMCGA
00572 *                                                                 ELXPMCGA
00573 *  DETERMINE SPOUSE DEPENDENT COVERAGE                            ELXPMCGA
00574 *                                                                 ELXPMCGA
00575 * CHANGE DEFAULT TO CALL. RGO 9/94                                ELXPMCGA
00576 ****************************************************************  ELXPMCGA
00577  4500-DTRMN-SPS-DEP-WP.                                           ELXPMCGA
00578      SET PMCI-PRE-EXIST-CALL TO TRUE.                             ELXPMCGA
00579                                                                   ELXPMCGA
00580      IF WS-SBSCRBR-WP                                             ELXPMCGA
00581         SET PMCI-PRE-EXIST-APPL TO TRUE.                          ELXPMCGA
00582 ****************************************************************  ELXPMCGA
00583 *                                                                 ELXPMCGA
00584 *  DETERMINE WAIVER INDICATOR                                     ELXPMCGA
00585 *                                                                 ELXPMCGA
00586 ****************************************************************  ELXPMCGA
00587  4600-DETERMINE-WAIVER.                                           ELXPMCGA
00588                                                                   ELXPMCGA
00589      EVALUATE TRUE                                                ELXPMCGA
00590         WHEN WS-WAIVE-NONE                                        ELXPMCGA
00591            SET PMCI-PRE-WAVE-NONE TO TRUE                         ELXPMCGA
00592         WHEN WS-WAIVE-INI-ENREE                                   ELXPMCGA
00593            SET PMCI-PRE-WAVE-INI-ENREE TO TRUE                    ELXPMCGA
00594         WHEN WS-WAIVE-INI-ENRMT                                   ELXPMCGA
00595            SET PMCI-PRE-WAVE-INI-ENRMT TO TRUE                    ELXPMCGA
00596         WHEN WS-WAIVE-CALL                                        ELXPMCGA
00597            SET PMCI-PRE-WAVE-CALL TO TRUE                         ELXPMCGA
00598         WHEN OTHER                                                ELXPMCGA
00599            SET PMCI-PRE-WAVE-CALL TO TRUE                         ELXPMCGA
00600      END-EVALUATE.                                                ELXPMCGA
00601                                                                   ELXPMCGA
00602 ****************************************************************  ELXPMCGA
00603 *                                                                 ELXPMCGA
00604 *  DETERMINE IF PRE-EXISTING CONDITIONS EXISTS                    ELXPMCGA
00605 *                                                                 ELXPMCGA
00606 ****************************************************************  ELXPMCGA
00607  4700-DETERMINE-OB-WAIT-PER.                                      ELXPMCGA
00608      SET PMCI-OBC-WAVE-NONE TO TRUE.                              ELXPMCGA
00609      IF PMCI-BSC-CNTRCT-GRP NOT EQUAL SPACES AND                  ELXPMCGA
00610                  PMCI-MM-CNTRCT-GRP NOT EQUAL SPACES              ELXPMCGA
00611         PERFORM 4710-DETERMINE-BOTH-OB                            ELXPMCGA
00612      ELSE                                                         ELXPMCGA
00613      IF PMCI-MM-CNTRCT-GRP = SPACES                               ELXPMCGA
00614         PERFORM 4720-DETERMINE-BASIC-OB                           ELXPMCGA
00615      ELSE                                                         ELXPMCGA
00616      IF PMCI-BSC-CNTRCT-GRP = SPACES                              ELXPMCGA
00617         PERFORM 4730-DETERMINE-MAJ-MED-OB.                        ELXPMCGA
00618 ****************************************************************  ELXPMCGA
00619 *                                                                 ELXPMCGA
00620 *  DETERMINE IF PRE-EXISTING CONDITIONS EXISTS FOR BOTH-OB        ELXPMCGA
00621 *                                                                 ELXPMCGA
00622 ****************************************************************  ELXPMCGA
00623  4710-DETERMINE-BOTH-OB.                                          ELXPMCGA
00624      IF PMCI-INSTITUTIONAL                                        ELXPMCGA
00625         IF GCG-BC-OB-WAITG-PERD-IND > ZERO OR                     ELXPMCGA
00626                          GCG-MM-OB-WAITG-PERD-IND > ZERO          ELXPMCGA
00627            PERFORM 4750-EVALUATE-OB-BC-BOTH-PREEX                 ELXPMCGA
00628         ELSE                                                      ELXPMCGA
00629            SET PMCI-PRE-EXIST-NOT-APPL TO TRUE                    ELXPMCGA
00630      ELSE                                                         ELXPMCGA
00631         IF PMCI-PROFESSIONAL                                      ELXPMCGA
00632            IF GCG-BS-OB-WAITG-PERD-IND > ZERO OR                  ELXPMCGA
00633                            GCG-MM-OB-WAITG-PERD-IND > ZERO        ELXPMCGA
00634               PERFORM 4760-EVALUATE-OB-BS-BOTH-PREEX              ELXPMCGA
00635            ELSE                                                   ELXPMCGA
00636               SET PMCI-PRE-EXIST-NOT-APPL TO TRUE.                ELXPMCGA
00637 ****************************************************************  ELXPMCGA
00638 *                                                                 ELXPMCGA
00639 *  DETERMINE IF PRE-EXISTING CONDITIONS EXISTS FOR BASIC-OB       ELXPMCGA
00640 *                                                                 ELXPMCGA
00641 ****************************************************************  ELXPMCGA
00642  4720-DETERMINE-BASIC-OB.                                         ELXPMCGA
00643      IF PMCI-MM-CNTRCT-GRP = SPACES                               ELXPMCGA
00644         IF PMCI-INSTITUTIONAL                                     ELXPMCGA
00645            IF GCG-BC-OB-WAITG-PERD-IND > ZERO                     ELXPMCGA
00646               SET PMCI-OBC-BSC TO TRUE                            ELXPMCGA
00647               PERFORM 4800-EVALUATE-OB-BC-PREEX                   ELXPMCGA
00648            ELSE                                                   ELXPMCGA
00649               CONTINUE                                            ELXPMCGA
00650         ELSE                                                      ELXPMCGA
00651         IF PMCI-PROFESSIONAL                                      ELXPMCGA
00652            IF GCG-BS-OB-WAITG-PERD-IND > ZERO                     ELXPMCGA
00653               SET PMCI-OBC-BSC TO TRUE                            ELXPMCGA
00654               PERFORM 4900-EVALUATE-OB-BS-PREEX                   ELXPMCGA
00655            ELSE                                                   ELXPMCGA
00656               CONTINUE.                                           ELXPMCGA
00657 ****************************************************************  ELXPMCGA
00658 *                                                                 ELXPMCGA
00659 *  DETERMINE IF PRE-EXISTING CONDITIONS EXISTS FOR MAJ MED-OB     ELXPMCGA
00660 *                                                                 ELXPMCGA
00661 ****************************************************************  ELXPMCGA
00662  4730-DETERMINE-MAJ-MED-OB.                                       ELXPMCGA
00663      IF PMCI-BSC-CNTRCT-GRP = SPACES                              ELXPMCGA
00664         IF GCG-MM-OB-WAITG-PERD-IND > ZERO                        ELXPMCGA
00665            SET PMCI-OBC-MM TO TRUE                                ELXPMCGA
00666            PERFORM 5000-EVALUATE-OB-MM-PREEX                      ELXPMCGA
00667         ELSE                                                      ELXPMCGA
00668            CONTINUE.                                              ELXPMCGA
00669 ****************************************************************  ELXPMCGA
00670 *                                                                 ELXPMCGA
00671 *  EVALUATE-OB WAITING PERIOD DAYS FOR BLUE CROSS OR MAJOR MED    ELXPMCGA
00672 *                                                                 ELXPMCGA
00673 ****************************************************************  ELXPMCGA
00674  4750-EVALUATE-OB-BC-BOTH-PREEX.                                  ELXPMCGA
00675         IF GCG-BC-OB-WAITG-PERD-MEM-DAYS > ZERO AND               ELXPMCGA
00676                         GCG-MM-OB-WAITG-PERD-MEM-DAYS > ZERO      ELXPMCGA
00677            SET PMCI-OBC-BSC-MM TO TRUE                            ELXPMCGA
00678            PERFORM 4800-EVALUATE-OB-BC-PREEX                      ELXPMCGA
00679      ELSE                                                         ELXPMCGA
00680         IF GCG-MM-OB-WAITG-PERD-MEM-DAYS > ZERO                   ELXPMCGA
00681            SET PMCI-OBC-MM TO TRUE                                ELXPMCGA
00682            PERFORM 5000-EVALUATE-OB-MM-PREEX                      ELXPMCGA
00683      ELSE                                                         ELXPMCGA
00684         SET PMCI-OBC-BSC TO TRUE                                  ELXPMCGA
00685         PERFORM 4800-EVALUATE-OB-BC-PREEX.                        ELXPMCGA
00686 ****************************************************************  ELXPMCGA
00687 *                                                                 ELXPMCGA
00688 *  EVALUATE-OB WAITING PERIOD DAYS FOR BLUE SHIELD OR MAJOR MED   ELXPMCGA
00689 *                                                                 ELXPMCGA
00690 ****************************************************************  ELXPMCGA
00691  4760-EVALUATE-OB-BS-BOTH-PREEX.                                  ELXPMCGA
00692         IF GCG-BS-OB-WAITG-PERD-MEM-DAYS > ZERO AND               ELXPMCGA
00693                         GCG-MM-OB-WAITG-PERD-MEM-DAYS > ZERO      ELXPMCGA
00694            SET PMCI-OBC-BSC-MM TO TRUE                            ELXPMCGA
00695            PERFORM 4900-EVALUATE-OB-BS-PREEX                      ELXPMCGA
00696      ELSE                                                         ELXPMCGA
00697         IF GCG-MM-OB-WAITG-PERD-MEM-DAYS > ZERO                   ELXPMCGA
00698            SET PMCI-OBC-MM TO TRUE                                ELXPMCGA
00699            PERFORM 5000-EVALUATE-OB-MM-PREEX                      ELXPMCGA
00700      ELSE                                                         ELXPMCGA
00701         SET PMCI-OBC-BSC TO TRUE                                  ELXPMCGA
00702         PERFORM 4900-EVALUATE-OB-BS-PREEX.                        ELXPMCGA
00703 ****************************************************************  ELXPMCGA
00704 *                                                                 ELXPMCGA
00705 *  EVALUATE-OB WAITING PERIOD DAYS FOR BLUE CROSS                 ELXPMCGA
00706 *  RGO 10/3/94                                                    ELXPMCGA
00707 ****************************************************************  ELXPMCGA
00708  4800-EVALUATE-OB-BC-PREEX.                                       ELXPMCGA
00709      MOVE GCG-BC-OB-WAITG-PERD-IND TO WS-HOLD-WP-IND.             ELXPMCGA
00710      MOVE GCG-BC-WAIVR-IND TO WS-WAIVER-IND.                      ELXPMCGA
00711      IF PMCI-MEMBER                                               ELXPMCGA
00712         IF GCG-BC-OB-WAITG-PERD-MEM-DAYS > ZERO                   ELXPMCGA
00713            MOVE GCG-BC-OB-WAITG-PERD-MEM-DAYS TO                  ELXPMCGA
00714                         PMCI-OB-NORM-COMP-WAIT-PRD                ELXPMCGA
00715         END-IF                                                    ELXPMCGA
00716      ELSE                                                         ELXPMCGA
00717      IF PMCI-SPOUSE                                               ELXPMCGA
00718         IF GCG-BC-OB-WAITG-PERD-SPS-DAYS > ZERO                   ELXPMCGA
00719            MOVE GCG-BC-OB-WAITG-PERD-SPS-DAYS TO                  ELXPMCGA
00720                         PMCI-OB-NORM-COMP-WAIT-PRD                ELXPMCGA
00721         END-IF                                                    ELXPMCGA
00722      ELSE                                                         ELXPMCGA
00723      IF PMCI-DEPENDENT                                            ELXPMCGA
00724         IF GCG-BC-OB-WAITG-PERD-DEP-DAYS > ZERO                   ELXPMCGA
00725            MOVE GCG-BC-OB-WAITG-PERD-DEP-DAYS TO                  ELXPMCGA
00726                         PMCI-OB-NORM-COMP-WAIT-PRD                ELXPMCGA
00727         END-IF                                                    ELXPMCGA
00728      END-IF.                                                      ELXPMCGA
00729                                                                   ELXPMCGA
00730      PERFORM 5100-DETERMINE-OB-WAIVER.                            ELXPMCGA
00731 ****************************************************************  ELXPMCGA
00732 *                                                                 ELXPMCGA
00733 *  EVALUATE-OB WAITING PERIOD DAYS FOR BLUE SHIELD                ELXPMCGA
00734 *                                                                 ELXPMCGA
00735 *  RGO 10/3/94                                                    ELXPMCGA
00736 ****************************************************************  ELXPMCGA
00737  4900-EVALUATE-OB-BS-PREEX.                                       ELXPMCGA
00738      MOVE GCG-BS-OB-WAITG-PERD-IND TO WS-HOLD-WP-IND.             ELXPMCGA
00739      MOVE GCG-BS-WAIVR-IND TO WS-WAIVER-IND.                      ELXPMCGA
00740      IF PMCI-MEMBER                                               ELXPMCGA
00741         IF GCG-BS-OB-WAITG-PERD-MEM-DAYS > ZERO                   ELXPMCGA
00742            MOVE GCG-BS-OB-WAITG-PERD-MEM-DAYS TO                  ELXPMCGA
00743                         PMCI-OB-NORM-COMP-WAIT-PRD                ELXPMCGA
00744         END-IF                                                    ELXPMCGA
00745      ELSE                                                         ELXPMCGA
00746      IF PMCI-SPOUSE                                               ELXPMCGA
00747         IF GCG-BS-OB-WAITG-PERD-SPS-DAYS > ZERO                   ELXPMCGA
00748            MOVE GCG-BS-OB-WAITG-PERD-SPS-DAYS TO                  ELXPMCGA
00749                         PMCI-OB-NORM-COMP-WAIT-PRD                ELXPMCGA
00750         END-IF                                                    ELXPMCGA
00751      ELSE                                                         ELXPMCGA
00752      IF PMCI-DEPENDENT                                            ELXPMCGA
00753         IF GCG-BS-OB-WAITG-PERD-DEP-DAYS > ZERO                   ELXPMCGA
00754            MOVE GCG-BS-OB-WAITG-PERD-DEP-DAYS TO                  ELXPMCGA
00755                         PMCI-OB-NORM-COMP-WAIT-PRD                ELXPMCGA
00756         END-IF                                                    ELXPMCGA
00757      END-IF.                                                      ELXPMCGA
00758                                                                   ELXPMCGA
00759      PERFORM 5100-DETERMINE-OB-WAIVER.                            ELXPMCGA
00760 ****************************************************************  ELXPMCGA
00761 *                                                                 ELXPMCGA
00762 *  EVALUATE-OB WAITING PERIOD DAYS FOR MAJOR MEDICAL              ELXPMCGA
00763 *  RGO 10/3/94                                                    ELXPMCGA
00764 ****************************************************************  ELXPMCGA
00765  5000-EVALUATE-OB-MM-PREEX.                                       ELXPMCGA
00766      MOVE GCG-MM-OB-WAITG-PERD-IND TO WS-HOLD-WP-IND.             ELXPMCGA
00767      MOVE GCG-MM-WAIVR-IND TO WS-WAIVER-IND.                      ELXPMCGA
00768      IF PMCI-MEMBER                                               ELXPMCGA
00769         IF GCG-MM-OB-WAITG-PERD-MEM-DAYS > ZERO                   ELXPMCGA
00770            MOVE GCG-MM-OB-WAITG-PERD-MEM-DAYS TO                  ELXPMCGA
00771                         PMCI-OB-NORM-COMP-WAIT-PRD                ELXPMCGA
00772         END-IF                                                    ELXPMCGA
00773      ELSE                                                         ELXPMCGA
00774      IF PMCI-SPOUSE                                               ELXPMCGA
00775         IF GCG-MM-OB-WAITG-PERD-SPS-DAYS > ZERO                   ELXPMCGA
00776            MOVE GCG-MM-OB-WAITG-PERD-SPS-DAYS TO                  ELXPMCGA
00777                         PMCI-OB-NORM-COMP-WAIT-PRD                ELXPMCGA
00778         END-IF                                                    ELXPMCGA
00779      ELSE                                                         ELXPMCGA
00780      IF PMCI-DEPENDENT                                            ELXPMCGA
00781         IF GCG-BS-OB-WAITG-PERD-DEP-DAYS > ZERO                   ELXPMCGA
00782            MOVE GCG-MM-OB-WAITG-PERD-DEP-DAYS TO                  ELXPMCGA
00783                         PMCI-OB-NORM-COMP-WAIT-PRD                ELXPMCGA
00784         END-IF                                                    ELXPMCGA
00785      END-IF.                                                      ELXPMCGA
00786                                                                   ELXPMCGA
00787      PERFORM 5100-DETERMINE-OB-WAIVER.                            ELXPMCGA
00788 ****************************************************************  ELXPMCGA
00789 *                                                                 ELXPMCGA
00790 *  DETERMINE WAIVER INDICATOR                                     ELXPMCGA
00791 *                                                                 ELXPMCGA
00792 ****************************************************************  ELXPMCGA
00793  5100-DETERMINE-OB-WAIVER.                                        ELXPMCGA
00794                                                                   ELXPMCGA
00795      EVALUATE TRUE                                                ELXPMCGA
00796         WHEN WS-WAIVE-NONE                                        ELXPMCGA
00797            SET PMCI-OBC-WAVE-NONE TO TRUE                         ELXPMCGA
00798         WHEN WS-WAIVE-INI-ENREE                                   ELXPMCGA
00799            SET PMCI-OBC-WAVE-INI-ENREE TO TRUE                    ELXPMCGA
00800         WHEN WS-WAIVE-INI-ENRMT                                   ELXPMCGA
00801            SET PMCI-OBC-WAVE-INI-ENRMT TO TRUE                    ELXPMCGA
00802         WHEN WS-WAIVE-CALL                                        ELXPMCGA
00803            SET PMCI-OBC-WAVE-CALL TO TRUE                         ELXPMCGA
00804         WHEN OTHER                                                ELXPMCGA
00805            SET PMCI-OBC-WAVE-CALL TO TRUE                         ELXPMCGA
00806      END-EVALUATE.                                                ELXPMCGA
00807                                                                   ELXPMCGA
00808 ****************************************************************  ELXPMCGA
00809  8000-DETERMINE-TIMELY-FILING.                                    ELXPMCGA
00810      MOVE GCG-TIMELY-FILG-IND TO WS-TIMELY-FILING.                ELXPMCGA
00811      EVALUATE TRUE                                                ELXPMCGA
00812         WHEN WS-NO-LIMIT                                          ELXPMCGA
00813            MOVE ZERO TO PMCI-TIMELY-FILING                        ELXPMCGA
00814            SET TIMELY-FILING-NO-LIMIT TO TRUE                     ELXPMCGA
00815         WHEN WS-2-MONTHS                                          ELXPMCGA
00816            MOVE WS-02 TO PMCI-TIMELY-FILING                       ELXPMCGA
00817            SET TIMELY-FILING-MONTHS TO TRUE                       ELXPMCGA
00818         WHEN WS-6-MONTHS                                          ELXPMCGA
00819            MOVE WS-06 TO PMCI-TIMELY-FILING                       ELXPMCGA
00820            SET TIMELY-FILING-MONTHS TO TRUE                       ELXPMCGA
00821         WHEN WS-15-MONTHS                                         ELXPMCGA
00822            MOVE WS-15 TO PMCI-TIMELY-FILING                       ELXPMCGA
00823            SET TIMELY-FILING-MONTHS TO TRUE                       ELXPMCGA
00824         WHEN WS-18-MONTHS                                         ELXPMCGA
00825            MOVE WS-18 TO PMCI-TIMELY-FILING                       ELXPMCGA
00826            SET TIMELY-FILING-MONTHS TO TRUE                       ELXPMCGA
00827         WHEN WS-24-MONTHS                                         ELXPMCGA
00828            MOVE WS-24 TO PMCI-TIMELY-FILING                       ELXPMCGA
00829            SET TIMELY-FILING-MONTHS TO TRUE                       ELXPMCGA
00830         WHEN WS-30-MONTHS                                         ELXPMCGA
00831            MOVE WS-30 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00832            SET TIMELY-FILING-MONTHS TO TRUE                       ELXPMCGA
00833         WHEN WS-36-MONTHS                                         ELXPMCGA
00834            MOVE WS-36 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00835            SET TIMELY-FILING-MONTHS TO TRUE                       ELXPMCGA
00836         WHEN WS-48-MONTHS                                         ELXPMCGA
00837            MOVE WS-48 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00838            SET TIMELY-FILING-MONTHS TO TRUE                       ELXPMCGA
00839         WHEN WS-60-MONTHS                                         ELXPMCGA
00840            MOVE WS-60 TO PMCI-CHC-REQUIREMENTS                    ELXPMCGA
00841            SET TIMELY-FILING-MONTHS TO TRUE                       ELXPMCGA
00842         WHEN OTHER                                                ELXPMCGA
00843            SET TIMELY-FILING-CALL TO TRUE                         ELXPMCGA
00844      END-EVALUATE.                                                ELXPMCGA
