00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELTWAITP
00003  PROGRAM-ID.         ELTWAITP.                                       LV001
00004                                                                   ELTWAITP
00005  AUTHOR.             LUCY TORRES                                  ELTWAITP
00006                                                                   ELTWAITP
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTWAITP
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTWAITP
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTWAITP
00010                      233 N. MICHIGAN AVE                          ELTWAITP
00011                      CHICAGO, ILLINOIS 60601                      ELTWAITP
00012                                                                   ELTWAITP
00013  DATE-WRITTEN.       04-16-1987.                                  ELTWAITP
00014                                                                   ELTWAITP
00015  DATE-COMPILED.                                                   ELTWAITP
00016                                                                   ELTWAITP
00017  SECURITY.           COPYRIGHT 1986,                              ELTWAITP
00018                      HEALTH CARE SERVICE CORPORATION              ELTWAITP
00019      SKIP3                                                        ELTWAITP
00020  ENVIRONMENT DIVISION.                                            ELTWAITP
00021                                                                   ELTWAITP
00022  CONFIGURATION SECTION.                                           ELTWAITP
00023  SOURCE-COMPUTER.    IBM-3090.                                    ELTWAITP
00024  OBJECT-COMPUTER.    IBM-3090.                                    ELTWAITP
00025      TITLE 'ELS WAITING PERIOD TOPIC'.                            ELTWAITP
00026 ******************************************************************ELTWAITP
00027 *                                                                *ELTWAITP
00028 *    DATE:       16-APR-1987                                     *ELTWAITP
00029 *    AUTHOR:     LUCY TORRES                                     *ELTWAITP
00030 *    FUNCTION:   TO CREATE AND DISPLAY THE 'WAIT AND EXPENSE     *ELTWAITP
00031 *                FREE PERIOD' INFORMATION FOR THE ENGLISH        *ELTWAITP
00032 *                LANGUAGE SYSTEM.                                *ELTWAITP
00033 *                                                                *ELTWAITP
00034 *    NOTES:   1) CICS LINKS ARE MADE TO THE FOLLOWING PROGRAMS:  *ELTWAITP
00035 *                  ELUCMIF  - CODES MANUAL INTERFACE MODULE      *ELTWAITP
00036 *                  ELUOUTPT - OUTPUT MODULE                      *ELTWAITP
00037 *                                                                *ELTWAITP
00038 *             2) ANY CHANGES MADE TO THE WS-FAMILY-RELATION-LEVEL*ELTWAITP
00039 *                SHOULD ALSO BE REFLECTED IN PRGRAM ELUKYSEL.    *ELTWAITP
00040 *                                                                *ELTWAITP
00041 ******************************************************************ELTWAITP
00042 *                                                                *ELTWAITP
00043 *                      MAINTENANCE HISTORY                       *ELTWAITP
00044 *                                                                *ELTWAITP
00045 *  MOD     DATE     BY  DRPT                ACTION               *ELTWAITP
00046 * ----- ----------- --- ----- ---------------------------------- *ELTWAITP
00047 * 01.00 16-APR-1987 LET  ---  CREATED                            *ELTWAITP
00048 * 02.00 15-APR-1988 NAC  ---  DISPLAY NUMBER OF EXPENSE FREE DAYS*ELTWAITP
00049 * 03.00 25-APR-1988 NAC  ---  RECONSTRUCT CHANGES MADE BY REB    *ELTWAITP
00050 *                             ON JUN 24 AND JUL 28 OF '87 BECAUSE*ELTWAITP
00051 *                             STRUCTURE VERSION WAS LOST.  PUT   *ELTWAITP
00052 *                             IN CHANGES FOR BC, BS, MM WAITING  *ELTWAITP
00053 *                             PERIOD DAYS FOR MEMBER, SPOUSE,    *ELTWAITP
00054 *                             DEPENDENT.  REMOVE LOGIC THAT      *ELTWAITP
00055 *                             CHECKED THE CM DESCRIPTION PTR FOR *ELTWAITP
00056 *                             NULLS.                             *ELTWAITP
00057 *                                                                *ELTWAITP
00058 * 04.00 05-MAY-1988 REB       CHANGED THE OUTPUT TO BE DISPLAYED *ELTWAITP
00059 *                             IN A TWO-COLUMN FORMAT AS REQUESTED*ELTWAITP
00060 *                                                                *ELTWAITP
00061 * 05.00 14-NOV-1989 EGL       DESTRUCTED PROGRAM AND ADDED NEW   *ELTWAITP
00062 *                             STORAGE MANAGEMENT CHANGES.        *ELTWAITP
00063 *                                                                *ELTWAITP
00064 * 06.00 02-MAY-1990 AKK       FOUND THAT THERE WERE NOT ENOUGH   *ELTWAITP
00065 *                             LINES ALLOWED FOR THE OUTPUT OF    *ELTWAITP
00066 *                             THE TEXT COMPRESSION/UNSTRING      *ELTWAITP
00067 *                             ROUTINE.  CORRECTED.               *ELTWAITP
00068 *                                                                *ELTWAITP
00069 * 07.00 24-SEP-1991 JPB       CHANGED WS-FAM-REL-LVL FROM 1 BYTE *ELTWAITP
00070 *                             TO TWO AND ADDED A LEADING ZERO    *ELTWAITP
00071 *                             TO ITS CORRESPONDING 88-LEVEL ITEMS*ELTWAITP
00072 * 07.01 24-SEP-1991 JPB       ADDED NEW FAMILY RELATIONSHIP      *ELTWAITP
00073 *                             VALUES TO TABLE.                   *ELTWAITP
00074 * 07.02 16-DEC-1991 RJL       ADDED NEW FAMILY RELATIONSHIP      *ELTWAITP
00075 *                             VALUES TO TABLE.                   *ELTWAITP
00076 *                                                                *ELTWAITP
00077 * 07.03 18-OCT-1993 RGO       ADDED NEW FAMILY RELATIONSHIP      *ELTWAITP
00078 *                             VALUES TO TABLE (VALUES 14 THRU    *ELTWAITP
00079 *                             30) PLUS PREVIOUS VALUES THAT      *ELTWAITP
00080 *                             NEEDED TO BE ADDED(10 THRU 13).    *ELTWAITP
00081 *                                                                *ELTWAITP
00082 * 07.04 26-SEP-1994 AKK       ADDED NEW FAMILY RELATIONSHIP      *ELTWAITP
00083 *                             VALUES TO TABLE (VALUES 31, 32).   *ELTWAITP
00084 * 07.05 15-MAY-2002 AKK       ADDED NEW FAMILY RELATIONSHIP      *ELTWAITP
00085 *                             VALUES TO TABLE (VALUES 31, 32).   *ELTWAITP
00084 * 07.05 04-DEC-2017 SRI       ADDED NEW FAMILY RELATIONSHIP      *ELTWAITP
00085 *                             VALUES TO TABLE (VALUES 0N, 39     *ELTWAITP
      *                             40 41 42 43 44 45 46 47).          *        
00086 ******************************************************************ELTWAITP
00087 /                                                                 ELTWAITP
00088  DATA DIVISION.                                                   ELTWAITP
00089  WORKING-STORAGE SECTION.                                         ELTWAITP
00090  01  WS-MISC.                                                     ELTWAITP
00091      05  WORK-STOR                PIC X(24) VALUE                 ELTWAITP
00092      'ELTWAITP WORKING STORAGE'.                                  ELTWAITP
00093      05  WS-PARA                  PIC X(04) VALUE ZEROS.          ELTWAITP
00094      05  WS-LOB-CONTRACT-LVL-IND  PIC XX.                         ELTWAITP
00095          88  BASIC-INST                     VALUES '01' '02' '03' ELTWAITP
00096                                                    '04' '07'.     ELTWAITP
00097          88  BASIC-PROF                     VALUES '02' '03' '05' ELTWAITP
00098                                                    '06' '07'.     ELTWAITP
00099          88  SUPPLEMENTAL                   VALUES '03' '04' '06' ELTWAITP
00100                                                    '08'.          ELTWAITP
00101      05  WS-BAR                   PIC X(01) VALUE '|'.            ELTWAITP
00102      05  WS-MASK-LINE.                                            ELTWAITP
00103          10  FILLER               PIC X(30) VALUE SPACES.         ELTWAITP
00104          10  FILLER               PIC X(01) VALUE '|'.            ELTWAITP
00105                                                                   ELTWAITP
00106  01  WS-HOLD-AREA.                                                ELTWAITP
00107      05  WS-I-P-S-TITLE           PIC  X(13) VALUE SPACES.        ELTWAITP
00108 /                                                                 ELTWAITP
00109  01  PROGRAM-CONSTANTS.                                           ELTWAITP
00110      05  PC-DAYS                  PIC  X(04) VALUE                ELTWAITP
00111              'DAYS'.                                              ELTWAITP
00112      05  PC-E                     PIC  X(01) VALUE                ELTWAITP
00113              'E'.                                                 ELTWAITP
00114      05  PC-INSTITUTIONAL         PIC  X(13) VALUE                ELTWAITP
00115              'INSTITUTIONAL'.                                     ELTWAITP
00116      05  PC-NOT-APPLICABLE        PIC  X(16) VALUE                ELTWAITP
00117              'NOT APPLICABLE '.                                   ELTWAITP
00118      05  PC-P                     PIC  X(01) VALUE                ELTWAITP
00119              'P'.                                                 ELTWAITP
00120      05  PC-PROFESSIONAL          PIC  X(13) VALUE                ELTWAITP
00121              'PROFESSIONAL'.                                      ELTWAITP
00122      05  PC-SUPPLEMENTAL          PIC  X(13) VALUE                ELTWAITP
00123              'SUPPLEMENTAL'.                                      ELTWAITP
00124 ***************************************************************   ELTWAITP
00125 **** H E A D E R  L I N E S  SUBJECT TO CHANGE                    ELTWAITP
00126 *************************************************************     ELTWAITP
00127  01  WS-HEADER-LINES.                                             ELTWAITP
00128      05  WS-HDR-LIN-3.                                            ELTWAITP
00129        10  FILLER                 PIC X(33) VALUE                 ELTWAITP
00130            'THE FOLLOWING BENEFITS ARE FOR:  '.                   ELTWAITP
00131        10  FILLER                 PIC X(29) VALUE                 ELTWAITP
00132            'WAITING/EXPENSE FREE PERIODS '.                       ELTWAITP
00133        10  WS-HDR-3-I-P-S         PIC X(13) VALUE SPACES.         ELTWAITP
00134 **************************************************************    ELTWAITP
00135 *** SCREEN BODY LINES                                             ELTWAITP
00136 **************************************************************    ELTWAITP
00137  01  WS-SCRN-LINS-AREA-A.                                         ELTWAITP
00138      05  WAIT-PERD-IND-SENT-1.                                    ELTWAITP
00139          10  FILLER               PIC X(30) VALUE                 ELTWAITP
00140              'WAITING PERIOD FOR '.                               ELTWAITP
00141      05  WAIT-PERD-IND-SENT-2.                                    ELTWAITP
00142          10  WP-IND-I-P-S         PIC X(13) VALUE SPACES.         ELTWAITP
00143          10  FILLER               PIC X(09) VALUE                 ELTWAITP
00144              ' BENEFITS'.                                         ELTWAITP
00145      05  WAIT-PERD-DAYS-SENT-1.                                   ELTWAITP
00146          10  WP-DAYS-I-P-S        PIC X(13) VALUE SPACES.         ELTWAITP
00147          10  FILLER               PIC X(15) VALUE                 ELTWAITP
00148              ' WAITING PERIOD'.                                   ELTWAITP
00149      05  WAIT-PERD-DAYS-MBR.                                      ELTWAITP
00150          10  FILLER               PIC X(11) VALUE                 ELTWAITP
00151              'MEMBER     '.                                       ELTWAITP
00152          10  WP-DAYS-MBR          PIC ZZ9.                        ELTWAITP
00153      05  WAIT-PERD-DAYS-SPS.                                      ELTWAITP
00154          10  FILLER               PIC X(11) VALUE                 ELTWAITP
00155              'SPOUSE     '.                                       ELTWAITP
00156          10  WP-DAYS-SPS          PIC ZZ9.                        ELTWAITP
00157      05  WAIT-PERD-DAYS-DEP.                                      ELTWAITP
00158          10  FILLER               PIC X(11) VALUE                 ELTWAITP
00159              'DEPENDENT  '.                                       ELTWAITP
00160          10  WP-DAYS-DEP          PIC ZZ9.                        ELTWAITP
00161                                                                   ELTWAITP
00162  01  WS-SCRN-LINS-AREA-B.                                         ELTWAITP
00163      05  WAIT-PERD-WAIVER-SENT.                                   ELTWAITP
00164          10  FILLER               PIC X(22) VALUE                 ELTWAITP
00165              'WAITING PERIOD WAIVER '.                            ELTWAITP
00166                                                                   ELTWAITP
00167  01  WS-SCRN-LINS-AREA-C.                                         ELTWAITP
00168      05  EXP-FREE-IND-SENT-1.                                     ELTWAITP
00169          10  FILLER               PIC X(24) VALUE                 ELTWAITP
00170              'EXPENSE FREE PERIOD FOR '.                          ELTWAITP
00171      05  EXP-FREE-IND-SENT-2.                                     ELTWAITP
00172          10  EF-IND-I-P-S         PIC X(13) VALUE SPACES.         ELTWAITP
00173          10  FILLER               PIC X(09) VALUE                 ELTWAITP
00174              ' BENEFITS'.                                         ELTWAITP
00175      05  EXP-FREE-DAYS.                                           ELTWAITP
00176          10  EF-IND-DAYS          PIC ZZ9.                        ELTWAITP
00177          10  FILLER               PIC X(08) VALUE ' DAYS.  '.     ELTWAITP
00178                                                                   ELTWAITP
00179  01  WS-SCRN-LINS-AREA-D.                                         ELTWAITP
00180      05  WAIT-PERD-OB-SENT-1.                                     ELTWAITP
00181          10  FILLER               PIC X(27) VALUE                 ELTWAITP
00182              'OBSTETRICAL WAITING PERIOD '.                       ELTWAITP
00183      05  WAIT-PERD-OB-SENT-2.                                     ELTWAITP
00184          10  WP-OB-I-P-S          PIC X(13) VALUE SPACES.         ELTWAITP
00185          10  FILLER               PIC X(09) VALUE                 ELTWAITP
00186              ' BENEFITS'.                                         ELTWAITP
00187      05  WAIT-PERD-OB-DAYS-SENT-1.                                ELTWAITP
00188          10  FILLER               PIC X(27) VALUE                 ELTWAITP
00189              'OBSTETRICAL WAITING PERIOD '.                       ELTWAITP
00190      05  WAIT-PERD-OB-DAYS-MBR.                                   ELTWAITP
00191          10  FILLER               PIC X(11) VALUE                 ELTWAITP
00192              'MEMBER     '.                                       ELTWAITP
00193          10  WP-OB-DAYS-MBR       PIC ZZ9.                        ELTWAITP
00194      05  WAIT-PERD-OB-DAYS-SPS.                                   ELTWAITP
00195          10  FILLER               PIC X(11) VALUE                 ELTWAITP
00196              'SPOUSE     '.                                       ELTWAITP
00197          10  WP-OB-DAYS-SPS       PIC ZZ9.                        ELTWAITP
00198      05  WAIT-PERD-OB-DAYS-DEP.                                   ELTWAITP
00199          10  FILLER               PIC X(11) VALUE                 ELTWAITP
00200              'DEPENDENT  '.                                       ELTWAITP
00201          10  WP-OB-DAYS-DEP       PIC ZZ9.                        ELTWAITP
00202 /                                                                 ELTWAITP
00203 ******************************************************************ELTWAITP
00204 *    THIS TABLE SHOULD BE CHANGED WHEN FAMILY VALUES ARE ADDED OR ELTWAITP
00205 *    CHANGED IN PROGRAM ELUKYSEL, AND VICE VERSA.                 ELTWAITP
00206 *    LAST CHANGED: 11/93, RGO.                                    ELTWAITP
00207 * ****************************************************************ELTWAITP
00208  01  WS-FAMILY-RELATION-LEVEL     PIC  X(02).                     ELTWAITP
00209      88  MEMBER-FAM-REL                       VALUES              ELTWAITP
00210                                   '0M' '00' '01' '03' '06' '07'   ELTWAITP
00211                                   '09' '0A' '0B' '0D' '0F' '0I'   ELTWAITP
00212                                   '0J' '0K' '0N'                  ELTWAITP
00212                                   '0R' '0S' '0T' '0U'             ELTWAITP
00213                                   '0V' '0W' '0X' '0Y' '0Z' '10'   ELTWAITP
00214                                   '11' '14' '15' '16' '17' '18'   ELTWAITP
00215                                   '19' '20' '21' '22' '23' '24'   ELTWAITP
00216                                   '25' '26' '27' '28' '29' '30'   ELTWAITP
00217                                   '31' '32' '33' '35' '36' '37'   ELTWAITP
00218                                   '38'  '39' '40' '41' '42' '43'  ELTWAITP
00218                                   '44'  '45' '46' '47'.           ELTWAITP
00219      88  SPOUSE-FAM-REL                       VALUES              ELTWAITP
00220                                   '0M' '00' '02' '03' '05' '06'   ELTWAITP
00221                                   '07' '08' '09' '0A' '0B' '0D'   ELTWAITP
00222                                   '0F' '0K' '0N'                  ELTWAITP
00222                                   '0R' '0S' '0T' '0U'             ELTWAITP
00223                                   '0V' '0W' '0X' '0Y' '0Z' '12'   ELTWAITP
00224                                   '13' '14' '15' '16' '17' '18'   ELTWAITP
00225                                   '19' '20' '21' '22' '23' '24'   ELTWAITP
00226                                   '25' '26' '27' '28' '29' '30'   ELTWAITP
00227                                   '31' '32' '33' '35' '36' '37'   ELTWAITP
00228                                   '38'  '39' '40' '41' '42' '43'  ELTWAITP
00229                                   '44'  '45' '46' '47'.           ELTWAITP
00230      88  DEPENDENT-FAM-REL                    VALUES              ELTWAITP
00231                                   '0M' '00' '04' '05' '06' '07'   ELTWAITP
00232                                   '08' '09' '0A' '0B' '0C' '0D'   ELTWAITP
00233                                   '0E' '0F' '0G' '0H' '0K' '0L'   ELTWAITP
00233                                   '0N'                            ELTWAITP
00234                                   '0P' '0R' '0S' '0T' '0U' '0V'   ELTWAITP
00235                                   '0W' '0X' '0Y' '0Z' '12'        ELTWAITP
00236                                   '13' '14' '15' '16' '17' '18'   ELTWAITP
00237                                   '19' '20' '21' '22' '23' '24'   ELTWAITP
00238                                   '25' '26' '27' '28' '29' '30'   ELTWAITP
00239                                   '31' '32' '33' '34' '35' '36'   ELTWAITP
00240                                   '37' '38' '39' '40' '41' '42'   ELTWAITP
00240                                   '43' '44' '45' '46' '47'.       ELTWAITP
00241                                                                   ELTWAITP
00242  01  WS-SWITCH.                                                   ELTWAITP
00243      05  WS-PERIOD-SW             PIC X(01)   VALUE 'P'.          ELTWAITP
00244          88  INSERT-PERIOD                    VALUE 'P'.          ELTWAITP
00245          88  NO-PERIOD-NEEDED                 VALUE 'N'.          ELTWAITP
00246                                                                   ELTWAITP
00247  01  WS-OUTPUT-AREA.                                              ELTWAITP
00248      05  WS-HEADINGS-CNT          PIC S9(04)  VALUE +0  COMP-3.   ELTWAITP
00249      05  WS-LINE-CNT              PIC S9(04)  VALUE +0  COMP-3.   ELTWAITP
00250      05  WS-OUTPUT                PIC X(1580) VALUE SPACES.       ELTWAITP
00251      05  WS-OUTPUT-ENTRY REDEFINES WS-OUTPUT                      ELTWAITP
00252                                   OCCURS 20 TIMES                 ELTWAITP
00253                                   INDEXED BY WS-OUTPUT-IDX.       ELTWAITP
00254          10  WS-OUTPUT-LINE.                                      ELTWAITP
00255              15  WS-HEADING       PIC X(30).                      ELTWAITP
00256              15  WS-DIVIDER       PIC X(01).                      ELTWAITP
00257              15  FILLER           PIC X(02).                      ELTWAITP
00258              15  WS-TEXT          PIC X(46).                      ELTWAITP
00259                                                                   ELTWAITP
00260 ***********************************************************       ELTWAITP
00261  LINKAGE SECTION.                                                 ELTWAITP
00262  01  DFHCOMMAREA.                                                 ELTWAITP
00263      COPY ELSCOMMC.                                               ELTWAITP
00264 /                                                                 ELTWAITP
00265      COPY ELSCIA2C.                                               ELTWAITP
00266 /                                                                 ELTWAITP
00267      COPY ELSCMDSC.                                               ELTWAITP
00268 /                                                                 ELTWAITP
00269      COPY ELSCMIFC.                                               ELTWAITP
00270 /                                                                 ELTWAITP
00271      COPY ELSIOPMC.                                               ELTWAITP
00272 /                                                                 ELTWAITP
00273      COPY ELSKEYSC.                                               ELTWAITP
00274 /                                                                 ELTWAITP
00275      COPY ELSOUTPC.                                               ELTWAITP
00276 /                                                                 ELTWAITP
00277      COPY ELSSRTPC.                                               ELTWAITP
00278 /                                                                 ELTWAITP
00279      COPY ELSTCWAC.                                               ELTWAITP
00280 /                                                                 ELTWAITP
00281      COPY ELSSSCBC.                                               ELTWAITP
00282 /                                                                 ELTWAITP
00283  01  ELR-GRP-REC-AREA.                                            ELTWAITP
00284      COPY GCGROUPC.                                               ELTWAITP
00285 /                                                                 ELTWAITP
00286  PROCEDURE DIVISION.                                              ELTWAITP
00287 ************************************************************      ELTWAITP
00288 *                                                          *      ELTWAITP
00289 *        WAITING PERIODS                                   *      ELTWAITP
00290 *                                                          *      ELTWAITP
00291 ************************************************************      ELTWAITP
00292  WAITING-PERIODS.                                                 ELTWAITP
00293      PERFORM INITIALIZE-MODULE.                                   ELTWAITP
00294      PERFORM MAIN-PROCESS.                                        ELTWAITP
00295      GOBACK.                                                      ELTWAITP
00296                                                                   ELTWAITP
00297                                                                   ELTWAITP
00298 ************************************************************      ELTWAITP
00299 *                                                          *      ELTWAITP
00300 *        INITIALIZE MODULE                                 *      ELTWAITP
00301 *                                                          *      ELTWAITP
00302 ************************************************************      ELTWAITP
00303  INITIALIZE-MODULE.                                               ELTWAITP
00304      PERFORM SET-ADDR-OF-CONTROL-BLOCKS.                          ELTWAITP
00305      PERFORM SET-ADDR-OF-WORK-AREAS.                              ELTWAITP
00306      PERFORM SET-ADDR-OF-GROUP-SPECIFIC-REC.                      ELTWAITP
00307                                                                   ELTWAITP
00308                                                                   ELTWAITP
00309 ************************************************************      ELTWAITP
00310 *                                                          *      ELTWAITP
00311 *        SET ADDR OF CONTROL BLOCKS                        *      ELTWAITP
00312 *                                                          *      ELTWAITP
00313 ************************************************************      ELTWAITP
00314  SET-ADDR-OF-CONTROL-BLOCKS.                                      ELTWAITP
00315      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELTWAITP
00316          EXEC CICS ABEND                                          ELTWAITP
00317                    ABCODE('EL01')                                 ELTWAITP
00318          END-EXEC.                                                ELTWAITP
00319                                                                   ELTWAITP
00320      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTWAITP
00321                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELTWAITP
00322      IF ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA = NULL           ELTWAITP
00323          EXEC CICS ABEND                                          ELTWAITP
00324                    ABCODE('EL02')                                 ELTWAITP
00325          END-EXEC.                                                ELTWAITP
00326                                                                   ELTWAITP
00327      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTWAITP
00328      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWAITP
00329                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELTWAITP
00330      IF CIA-RC-PTR-NULL                                           ELTWAITP
00331          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWAITP
00332 /***********************************************************      ELTWAITP
00333 *                                                          *      ELTWAITP
00334 *        SET ADDR OF WORK AREAS                            *      ELTWAITP
00335 *                                                          *      ELTWAITP
00336 ************************************************************      ELTWAITP
00337  SET-ADDR-OF-WORK-AREAS.                                          ELTWAITP
00338      PERFORM SET-ADDR-OF-CODES-MANUAL-INTER.                      ELTWAITP
00339      PERFORM SET-ADDR-OF-OUTPUT-INTERFACE.                        ELTWAITP
00340      PERFORM SET-ADDR-OF-TEXT-WORK-AREA.                          ELTWAITP
00341      PERFORM SET-ADDR-OF-KEY-WORK-AREA.                           ELTWAITP
00342                                                                   ELTWAITP
00343                                                                   ELTWAITP
00344  SET-ADDR-OF-CODES-MANUAL-INTER.                                  ELTWAITP
00345      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTWAITP
00346      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWAITP
00347                 ADDRESS OF CMF-CODES-MANUAL-INTERFACE.            ELTWAITP
00348      IF CIA-RC-PTR-NULL                                           ELTWAITP
00349          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWAITP
00350                                                                   ELTWAITP
00351                                                                   ELTWAITP
00352  SET-ADDR-OF-OUTPUT-INTERFACE.                                    ELTWAITP
00353      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTWAITP
00354      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWAITP
00355                 ADDRESS OF COF-OUTPUT-INTERFACE.                  ELTWAITP
00356      IF CIA-RC-PTR-NULL                                           ELTWAITP
00357          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWAITP
00358                                                                   ELTWAITP
00359                                                                   ELTWAITP
00360  SET-ADDR-OF-TEXT-WORK-AREA.                                      ELTWAITP
00361      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTWAITP
00362      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWAITP
00363                 ADDRESS OF TCAR-COMPRESSION-WORK-AREA.            ELTWAITP
00364      IF CIA-RC-PTR-NULL                                           ELTWAITP
00365          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWAITP
00366                                                                   ELTWAITP
00367                                                                   ELTWAITP
00368  SET-ADDR-OF-KEY-WORK-AREA.                                       ELTWAITP
00369      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTWAITP
00370      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWAITP
00371                 ADDRESS OF KWA-FILE-KEY-WORK-AREA.                ELTWAITP
00372      IF CIA-RC-PTR-NULL                                           ELTWAITP
00373          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTWAITP
00374 /***********************************************************      ELTWAITP
00375 *                                                          *      ELTWAITP
00376 *        SET ADDR OF GROUP SPECIFIC REC                    *      ELTWAITP
00377 *                                                          *      ELTWAITP
00378 ************************************************************      ELTWAITP
00379  SET-ADDR-OF-GROUP-SPECIFIC-REC.                                  ELTWAITP
00380      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTWAITP
00381      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWAITP
00382                 ADDRESS OF ELR-GRP-REC-AREA.                      ELTWAITP
00383      IF CIA-RC-PTR-NULL                                           ELTWAITP
00384          SET CIA-AB-NOTFND-GCGRPSPC  TO  TRUE                     ELTWAITP
00385          PERFORM SIGNAL-ABEND.                                    ELTWAITP
00386 /***********************************************************      ELTWAITP
00387 *                                                          *      ELTWAITP
00388 *        MAIN PROCESS                                      *      ELTWAITP
00389 *                                                          *      ELTWAITP
00390 ************************************************************      ELTWAITP
00391  MAIN-PROCESS.                                                    ELTWAITP
00392      SET  WS-OUTPUT-IDX    TO +1.                                 ELTWAITP
00393      MOVE WS-MASK-LINE     TO COF-MASK-LINE.                      ELTWAITP
00394      MOVE 'GROUP'          TO CMF-RECORD-PREFIX.                  ELTWAITP
00395      MOVE GCG-FAM-REL-LVL  TO WS-FAMILY-RELATION-LEVEL.           ELTWAITP
00396      MOVE GCG-L-O-B-CONTRACT-LEVEL-IND                            ELTWAITP
00397                            TO  WS-LOB-CONTRACT-LVL-IND.           ELTWAITP
00398      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTWAITP
00399                AND (BASIC-INST)                                   ELTWAITP
00400          PERFORM SET-INST-TITLE-AND-PROCESS-BCX                   ELTWAITP
00401      END-IF.                                                      ELTWAITP
00402      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTWAITP
00403                 AND (BASIC-PROF)                                  ELTWAITP
00404          PERFORM SET-PROF-TITLE-AND-PROCESS-BSX                   ELTWAITP
00405      END-IF.                                                      ELTWAITP
00406      IF SUPPLEMENTAL                                              ELTWAITP
00407          PERFORM SET-SUPPLEMENTAL-TITLE-AND-PRO                   ELTWAITP
00408      END-IF.                                                      ELTWAITP
00409      PERFORM END-THE-DISPLAY.                                     ELTWAITP
00410                                                                   ELTWAITP
00411                                                                   ELTWAITP
00412 ************************************************************      ELTWAITP
00413 *                                                          *      ELTWAITP
00414 *        SET INST TITLE AND PROCESS BC SCREEN              *      ELTWAITP
00415 *                                                          *      ELTWAITP
00416 ************************************************************      ELTWAITP
00417  SET-INST-TITLE-AND-PROCESS-BCX.                                  ELTWAITP
00418      MOVE PC-INSTITUTIONAL  TO  WS-I-P-S-TITLE.                   ELTWAITP
00419      PERFORM DISPLAY-THE-HEADER.                                  ELTWAITP
00420      PERFORM PROCESS-BC-SCREEN.                                   ELTWAITP
00421                                                                   ELTWAITP
00422                                                                   ELTWAITP
00423 ************************************************************      ELTWAITP
00424 *                                                          *      ELTWAITP
00425 *        SET PROF TITLE AND PROCESS BS SCREEN              *      ELTWAITP
00426 *                                                          *      ELTWAITP
00427 ************************************************************      ELTWAITP
00428  SET-PROF-TITLE-AND-PROCESS-BSX.                                  ELTWAITP
00429      MOVE PC-PROFESSIONAL  TO  WS-I-P-S-TITLE.                    ELTWAITP
00430      PERFORM DISPLAY-THE-HEADER.                                  ELTWAITP
00431      PERFORM PROCESS-BS-SCREEN.                                   ELTWAITP
00432                                                                   ELTWAITP
00433                                                                   ELTWAITP
00434 ************************************************************      ELTWAITP
00435 *                                                          *      ELTWAITP
00436 *        SET SUPPLEMENTAL TITLE AND PROCESS MM SCREEN      *      ELTWAITP
00437 *                                                          *      ELTWAITP
00438 ************************************************************      ELTWAITP
00439  SET-SUPPLEMENTAL-TITLE-AND-PRO.                                  ELTWAITP
00440      MOVE PC-SUPPLEMENTAL  TO  WS-I-P-S-TITLE.                    ELTWAITP
00441      PERFORM DISPLAY-THE-HEADER.                                  ELTWAITP
00442      PERFORM PROCESS-MM-SCREEN.                                   ELTWAITP
00443 /***********************************************************      ELTWAITP
00444 *                                                          *      ELTWAITP
00445 *        END THE DISPLAY                                   *      ELTWAITP
00446 *                                                          *      ELTWAITP
00447 ************************************************************      ELTWAITP
00448  END-THE-DISPLAY.                                                 ELTWAITP
00449      MOVE PC-E   TO  COF-FUNCTION.                                ELTWAITP
00450      MOVE ZEROS  TO  COF-NBR-HDR-LINES,                           ELTWAITP
00451                      COF-NBR-DTL-LINES.                           ELTWAITP
00452      PERFORM LINK-TO-OUTPUT-MODULE.                               ELTWAITP
00453                                                                   ELTWAITP
00454                                                                   ELTWAITP
00455 ************************************************************      ELTWAITP
00456 *                                                          *      ELTWAITP
00457 *        DISPLAY THE HEADER                                *      ELTWAITP
00458 *                                                          *      ELTWAITP
00459 ************************************************************      ELTWAITP
00460  DISPLAY-THE-HEADER.                                              ELTWAITP
00461      MOVE WS-I-P-S-TITLE  TO  WS-HDR-3-I-P-S,                     ELTWAITP
00462                               WP-IND-I-P-S                        ELTWAITP
00463                               WP-DAYS-I-P-S                       ELTWAITP
00464                               EF-IND-I-P-S                        ELTWAITP
00465                               WP-OB-I-P-S.                        ELTWAITP
00466      MOVE PC-P            TO  COF-FUNCTION.                       ELTWAITP
00467      MOVE +0              TO  COF-NBR-DTL-LINES.                  ELTWAITP
00468      MOVE +3              TO  COF-NBR-HDR-LINES.                  ELTWAITP
00469      MOVE WS-HDR-LIN-3    TO  COF-HDR-LINE (2).                   ELTWAITP
00470      MOVE ALL '-'         TO  COF-HDR-LINE (3).                   ELTWAITP
00471      PERFORM LINK-TO-OUTPUT-MODULE.                               ELTWAITP
00472 /***********************************************************      ELTWAITP
00473 *                                                          *      ELTWAITP
00474 *        PROCESS BC SCREEN                                 *      ELTWAITP
00475 *                                                          *      ELTWAITP
00476 ************************************************************      ELTWAITP
00477  PROCESS-BC-SCREEN.                                               ELTWAITP
00478      IF GCG-BC-WAITG-PERD-IND = ZERO OR SPACE                     ELTWAITP
00479          PERFORM SETUP-NOT-APPLICABLE-MESSAGE                     ELTWAITP
00480      ELSE                                                         ELTWAITP
00481          PERFORM SETUP-BC-SENTENCES.                              ELTWAITP
00482      PERFORM DISPLAY-DASHED-LINE-AT-END-OFX.                      ELTWAITP
00483                                                                   ELTWAITP
00484                                                                   ELTWAITP
00485 ************************************************************      ELTWAITP
00486 *                                                          *      ELTWAITP
00487 *        SETUP BC SENTENCES                                *      ELTWAITP
00488 *                                                          *      ELTWAITP
00489 ************************************************************      ELTWAITP
00490  SETUP-BC-SENTENCES.                                              ELTWAITP
00491      PERFORM SETUP-BC-WAIT-PERIOD-IND-SENTE.                      ELTWAITP
00492      PERFORM SETUP-BC-WAIT-PERIOD-DAYS-SENT.                      ELTWAITP
00493      IF GCG-BC-WAIVR-IND NOT = ZERO AND SPACE                     ELTWAITP
00494          PERFORM SETUP-BC-WAIVER-SENTENCE.                        ELTWAITP
00495      IF GCG-BC-EXPENSE-FREE-IND NOT = ZERO AND SPACE              ELTWAITP
00496          PERFORM SETUP-BC-EXPENSE-FREE-SENTENCE.                  ELTWAITP
00497      IF GCG-BC-OB-WAITG-PERD-IND NOT = ZERO AND SPACE             ELTWAITP
00498          PERFORM SETUP-BC-OB-WAIT-PERIOD-SENTEN.                  ELTWAITP
00499                                                                   ELTWAITP
00500                                                                   ELTWAITP
00501 ************************************************************      ELTWAITP
00502 *                                                          *      ELTWAITP
00503 *        SETUP BC WAIT PERIOD IND SENTENCE                 *      ELTWAITP
00504 *                                                          *      ELTWAITP
00505 ************************************************************      ELTWAITP
00506  SETUP-BC-WAIT-PERIOD-IND-SENTE.                                  ELTWAITP
00507      MOVE GCG-BC-WAITG-PERD-IND  TO  CMF-CODE-VALUE.              ELTWAITP
00508      MOVE 'BC-WAITG-PERD-IND'    TO                               ELTWAITP
00509          CMF-ELEMENT-SYSTEM-NAME.                                 ELTWAITP
00510      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00511      MOVE WAIT-PERD-IND-SENT-1   TO  WS-HEADING (1).              ELTWAITP
00512      MOVE WAIT-PERD-IND-SENT-2   TO  WS-HEADING (2).              ELTWAITP
00513      MOVE +2                     TO  WS-HEADINGS-CNT.             ELTWAITP
00514      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00515                                                                   ELTWAITP
00516                                                                   ELTWAITP
00517 ************************************************************      ELTWAITP
00518 *                                                          *      ELTWAITP
00519 *        SETUP BC WAIT PERIOD DAYS SENTENCE                *      ELTWAITP
00520 *                                                          *      ELTWAITP
00521 ************************************************************      ELTWAITP
00522  SETUP-BC-WAIT-PERIOD-DAYS-SENT.                                  ELTWAITP
00523      MOVE ZEROS  TO  WP-DAYS-MBR,                                 ELTWAITP
00524                      WP-DAYS-SPS,                                 ELTWAITP
00525                      WP-DAYS-DEP.                                 ELTWAITP
00526      IF GCG-BC-WAITG-PERD-MEM-DAYS NUMERIC                        ELTWAITP
00527          MOVE GCG-BC-WAITG-PERD-MEM-DAYS  TO  WP-DAYS-MBR.        ELTWAITP
00528      IF GCG-BC-WAITG-PERD-SPS-DAYS NUMERIC                        ELTWAITP
00529          MOVE GCG-BC-WAITG-PERD-SPS-DAYS  TO  WP-DAYS-SPS.        ELTWAITP
00530      IF GCG-BC-WAITG-PERD-DEP-DAYS NUMERIC                        ELTWAITP
00531          MOVE GCG-BC-WAITG-PERD-DEP-DAYS  TO  WP-DAYS-DEP.        ELTWAITP
00532      MOVE WAIT-PERD-DAYS-SENT-1  TO  WS-HEADING (1).              ELTWAITP
00533      MOVE PC-DAYS                TO  WS-HEADING (2).              ELTWAITP
00534      MOVE +2                     TO  WS-HEADINGS-CNT.             ELTWAITP
00535      IF MEMBER-FAM-REL                                            ELTWAITP
00536          PERFORM SHOW-MEMBER-WAIT-DAYS.                           ELTWAITP
00537      IF SPOUSE-FAM-REL                                            ELTWAITP
00538          PERFORM SHOW-SPOUSE-WAIT-DAYS.                           ELTWAITP
00539      IF DEPENDENT-FAM-REL                                         ELTWAITP
00540          PERFORM SHOW-DEPENDENT-WAIT-DAYS.                        ELTWAITP
00541      PERFORM DISPLAY-INFORMATION-STORED-INX.                      ELTWAITP
00542                                                                   ELTWAITP
00543                                                                   ELTWAITP
00544 ************************************************************      ELTWAITP
00545 *                                                          *      ELTWAITP
00546 *        SETUP BC WAIVER SENTENCE                          *      ELTWAITP
00547 *                                                          *      ELTWAITP
00548 ************************************************************      ELTWAITP
00549  SETUP-BC-WAIVER-SENTENCE.                                        ELTWAITP
00550      MOVE GCG-BC-WAIVR-IND   TO  CMF-CODE-VALUE.                  ELTWAITP
00551      MOVE 'BC-WAIVR-IND'     TO  CMF-ELEMENT-SYSTEM-NAME.         ELTWAITP
00552      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00553      MOVE WAIT-PERD-WAIVER-SENT  TO  WS-HEADING (WS-OUTPUT-IDX).  ELTWAITP
00554      MOVE +1                     TO  WS-HEADINGS-CNT.             ELTWAITP
00555      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00556                                                                   ELTWAITP
00557                                                                   ELTWAITP
00558 ************************************************************      ELTWAITP
00559 *                                                          *      ELTWAITP
00560 *        SETUP BC EXPENSE FREE SENTENCE                    *      ELTWAITP
00561 *                                                          *      ELTWAITP
00562 ************************************************************      ELTWAITP
00563  SETUP-BC-EXPENSE-FREE-SENTENCE.                                  ELTWAITP
00564      MOVE GCG-BC-EXPENSE-FREE-IND  TO  CMF-CODE-VALUE.            ELTWAITP
00565      MOVE 'BC-EXPENSE-FREE-IND'    TO  CMF-ELEMENT-SYSTEM-NAME.   ELTWAITP
00566      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00567      MOVE GCG-BC-EXPENSE-FREE-DAYS TO EF-IND-DAYS.                ELTWAITP
00568      MOVE EXP-FREE-DAYS            TO WS-TEXT (1).                ELTWAITP
00569      MOVE +1                       TO WS-LINE-CNT.                ELTWAITP
00570      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
00571      MOVE EXP-FREE-IND-SENT-1      TO WS-HEADING (1).             ELTWAITP
00572      MOVE EXP-FREE-IND-SENT-2      TO WS-HEADING (2).             ELTWAITP
00573      MOVE +2                       TO WS-HEADINGS-CNT.            ELTWAITP
00574      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00575                                                                   ELTWAITP
00576                                                                   ELTWAITP
00577 ************************************************************      ELTWAITP
00578 *                                                          *      ELTWAITP
00579 *        SETUP BC OB WAIT PERIOD SENTENCE                  *      ELTWAITP
00580 *                                                          *      ELTWAITP
00581 ************************************************************      ELTWAITP
00582  SETUP-BC-OB-WAIT-PERIOD-SENTEN.                                  ELTWAITP
00583      MOVE GCG-BC-OB-WAITG-PERD-IND  TO  CMF-CODE-VALUE.           ELTWAITP
00584      MOVE 'BC-OB-WAITG-PERD-IND'    TO  CMF-ELEMENT-SYSTEM-NAME.  ELTWAITP
00585      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00586      MOVE WAIT-PERD-OB-SENT-1       TO WS-HEADING (1).            ELTWAITP
00587      MOVE WAIT-PERD-OB-SENT-2       TO WS-HEADING (2).            ELTWAITP
00588      MOVE +2                        TO WS-HEADINGS-CNT.           ELTWAITP
00589      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00590      MOVE WAIT-PERD-OB-DAYS-SENT-1  TO WS-HEADING (1).            ELTWAITP
00591      MOVE PC-DAYS                   TO WS-HEADING (2).            ELTWAITP
00592      MOVE +2                        TO WS-HEADINGS-CNT.           ELTWAITP
00593      MOVE ZEROS  TO  WP-OB-DAYS-MBR,                              ELTWAITP
00594                      WP-OB-DAYS-SPS,                              ELTWAITP
00595                      WP-OB-DAYS-DEP.                              ELTWAITP
00596      IF GCG-BC-OB-WAITG-PERD-MEM-DAYS NUMERIC AND                 ELTWAITP
00597                MEMBER-FAM-REL                                     ELTWAITP
00598          PERFORM MOVE-MBR-BC-OB-WAITING-DAYS.                     ELTWAITP
00599      IF GCG-BC-OB-WAITG-PERD-SPS-DAYS NUMERIC AND                 ELTWAITP
00600                SPOUSE-FAM-REL                                     ELTWAITP
00601          PERFORM MOVE-SPS-BC-OB-WAITING-DAYS.                     ELTWAITP
00602      IF GCG-BC-OB-WAITG-PERD-DEP-DAYS NUMERIC AND                 ELTWAITP
00603                DEPENDENT-FAM-REL                                  ELTWAITP
00604          PERFORM MOVE-DEP-BC-OB-WAITING-DAYS.                     ELTWAITP
00605      PERFORM DISPLAY-INFORMATION-STORED-INX.                      ELTWAITP
00606                                                                   ELTWAITP
00607                                                                   ELTWAITP
00608 ************************************************************      ELTWAITP
00609 *                                                          *      ELTWAITP
00610 *        MOVE MBR BC OB WAITING DAYS                       *      ELTWAITP
00611 *                                                          *      ELTWAITP
00612 ************************************************************      ELTWAITP
00613  MOVE-MBR-BC-OB-WAITING-DAYS.                                     ELTWAITP
00614      MOVE GCG-BC-OB-WAITG-PERD-MEM-DAYS                           ELTWAITP
00615                TO  WP-OB-DAYS-MBR.                                ELTWAITP
00616      ADD  +1   TO  WS-LINE-CNT.                                   ELTWAITP
00617      MOVE WAIT-PERD-OB-DAYS-MBR                                   ELTWAITP
00618                TO  WS-TEXT (WS-OUTPUT-IDX).                       ELTWAITP
00619      SET WS-OUTPUT-IDX UP BY +1.                                  ELTWAITP
00620                                                                   ELTWAITP
00621                                                                   ELTWAITP
00622 ************************************************************      ELTWAITP
00623 *                                                          *      ELTWAITP
00624 *        MOVE SPS BC OB WAITING DAYS                       *      ELTWAITP
00625 *                                                          *      ELTWAITP
00626 ************************************************************      ELTWAITP
00627  MOVE-SPS-BC-OB-WAITING-DAYS.                                     ELTWAITP
00628      MOVE GCG-BC-OB-WAITG-PERD-SPS-DAYS                           ELTWAITP
00629                TO  WP-OB-DAYS-SPS.                                ELTWAITP
00630      ADD  +1   TO  WS-LINE-CNT.                                   ELTWAITP
00631      MOVE WAIT-PERD-OB-DAYS-SPS                                   ELTWAITP
00632                TO  WS-TEXT (WS-OUTPUT-IDX).                       ELTWAITP
00633      SET WS-OUTPUT-IDX UP BY +1.                                  ELTWAITP
00634                                                                   ELTWAITP
00635                                                                   ELTWAITP
00636 ************************************************************      ELTWAITP
00637 *                                                          *      ELTWAITP
00638 *        MOVE DEP BC OB WAITING DAYS                       *      ELTWAITP
00639 *                                                          *      ELTWAITP
00640 ************************************************************      ELTWAITP
00641  MOVE-DEP-BC-OB-WAITING-DAYS.                                     ELTWAITP
00642      MOVE GCG-BC-OB-WAITG-PERD-DEP-DAYS                           ELTWAITP
00643                TO  WP-OB-DAYS-DEP.                                ELTWAITP
00644      ADD  +1   TO  WS-LINE-CNT.                                   ELTWAITP
00645      MOVE WAIT-PERD-OB-DAYS-DEP                                   ELTWAITP
00646                TO  WS-TEXT (WS-OUTPUT-IDX).                       ELTWAITP
00647      SET WS-OUTPUT-IDX UP BY +1.                                  ELTWAITP
00648 /***********************************************************      ELTWAITP
00649 *                                                          *      ELTWAITP
00650 *        PROCESS BS SCREEN                                 *      ELTWAITP
00651 *                                                          *      ELTWAITP
00652 ************************************************************      ELTWAITP
00653  PROCESS-BS-SCREEN.                                               ELTWAITP
00654      IF GCG-BS-WAITG-PERD-IND = ZERO OR SPACE                     ELTWAITP
00655          PERFORM SETUP-NOT-APPLICABLE-MESSAGE                     ELTWAITP
00656      ELSE                                                         ELTWAITP
00657          PERFORM SETUP-BS-SENTENCES.                              ELTWAITP
00658      PERFORM DISPLAY-DASHED-LINE-AT-END-OFX.                      ELTWAITP
00659                                                                   ELTWAITP
00660                                                                   ELTWAITP
00661 ************************************************************      ELTWAITP
00662 *                                                          *      ELTWAITP
00663 *        SETUP BS SENTENCES                                *      ELTWAITP
00664 *                                                          *      ELTWAITP
00665 ************************************************************      ELTWAITP
00666  SETUP-BS-SENTENCES.                                              ELTWAITP
00667      PERFORM SETUP-BS-WAIT-PERIOD-IND-SENTE.                      ELTWAITP
00668      PERFORM SETUP-BS-WAIT-PERIOD-DAYS-SENT.                      ELTWAITP
00669      IF GCG-BS-WAIVR-IND NOT = ZERO AND SPACE                     ELTWAITP
00670          PERFORM SETUP-BS-WAIVER-SENTENCE.                        ELTWAITP
00671      IF GCG-BS-EXPENSE-FREE-IND NOT = ZERO AND SPACE              ELTWAITP
00672          PERFORM SETUP-BS-EXPENSE-FREE-SENTENCE.                  ELTWAITP
00673      IF GCG-BS-OB-WAITG-PERD-IND NOT = ZERO AND SPACE             ELTWAITP
00674          PERFORM SETUP-BS-OB-WAIT-PERIOD-SENTEN.                  ELTWAITP
00675                                                                   ELTWAITP
00676                                                                   ELTWAITP
00677 ************************************************************      ELTWAITP
00678 *                                                          *      ELTWAITP
00679 *        SETUP BS WAIT PERIOD IND SENTENCE                 *      ELTWAITP
00680 *                                                          *      ELTWAITP
00681 ************************************************************      ELTWAITP
00682  SETUP-BS-WAIT-PERIOD-IND-SENTE.                                  ELTWAITP
00683      MOVE GCG-BS-WAITG-PERD-IND  TO  CMF-CODE-VALUE.              ELTWAITP
00684      MOVE 'BS-WAITG-PERD-IND'    TO                               ELTWAITP
00685          CMF-ELEMENT-SYSTEM-NAME.                                 ELTWAITP
00686      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00687      MOVE WAIT-PERD-IND-SENT-1   TO  WS-HEADING (1).              ELTWAITP
00688      MOVE WAIT-PERD-IND-SENT-2   TO  WS-HEADING (2).              ELTWAITP
00689      MOVE +2                     TO  WS-HEADINGS-CNT.             ELTWAITP
00690      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00691                                                                   ELTWAITP
00692                                                                   ELTWAITP
00693 ************************************************************      ELTWAITP
00694 *                                                          *      ELTWAITP
00695 *        SETUP BS WAIT PERIOD DAYS SENTENCE                *      ELTWAITP
00696 *                                                          *      ELTWAITP
00697 ************************************************************      ELTWAITP
00698  SETUP-BS-WAIT-PERIOD-DAYS-SENT.                                  ELTWAITP
00699      MOVE ZEROS  TO  WP-DAYS-MBR,                                 ELTWAITP
00700                      WP-DAYS-SPS,                                 ELTWAITP
00701                      WP-DAYS-DEP.                                 ELTWAITP
00702      IF GCG-BS-WAITG-PERD-MEM-DAYS NUMERIC                        ELTWAITP
00703          MOVE GCG-BS-WAITG-PERD-MEM-DAYS  TO  WP-DAYS-MBR.        ELTWAITP
00704      IF GCG-BS-WAITG-PERD-SPS-DAYS NUMERIC                        ELTWAITP
00705          MOVE GCG-BS-WAITG-PERD-SPS-DAYS  TO  WP-DAYS-SPS.        ELTWAITP
00706      IF GCG-BS-WAITG-PERD-DEP-DAYS NUMERIC                        ELTWAITP
00707          MOVE GCG-BS-WAITG-PERD-DEP-DAYS  TO  WP-DAYS-DEP.        ELTWAITP
00708      MOVE WAIT-PERD-DAYS-SENT-1  TO  WS-HEADING (1).              ELTWAITP
00709      MOVE PC-DAYS                TO  WS-HEADING(2).               ELTWAITP
00710      MOVE +2                     TO  WS-HEADINGS-CNT.             ELTWAITP
00711      IF MEMBER-FAM-REL                                            ELTWAITP
00712          PERFORM SHOW-MEMBER-WAIT-DAYS.                           ELTWAITP
00713      IF SPOUSE-FAM-REL                                            ELTWAITP
00714          PERFORM SHOW-SPOUSE-WAIT-DAYS.                           ELTWAITP
00715      IF DEPENDENT-FAM-REL                                         ELTWAITP
00716          PERFORM SHOW-DEPENDENT-WAIT-DAYS.                        ELTWAITP
00717      PERFORM DISPLAY-INFORMATION-STORED-INX.                      ELTWAITP
00718                                                                   ELTWAITP
00719                                                                   ELTWAITP
00720 ************************************************************      ELTWAITP
00721 *                                                          *      ELTWAITP
00722 *        SETUP BS WAIVER SENTENCE                          *      ELTWAITP
00723 *                                                          *      ELTWAITP
00724 ************************************************************      ELTWAITP
00725  SETUP-BS-WAIVER-SENTENCE.                                        ELTWAITP
00726      MOVE GCG-BS-WAIVR-IND   TO  CMF-CODE-VALUE.                  ELTWAITP
00727      MOVE 'BS-WAIVR-IND'     TO                                   ELTWAITP
00728          CMF-ELEMENT-SYSTEM-NAME.                                 ELTWAITP
00729      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00730      MOVE WAIT-PERD-WAIVER-SENT  TO  WS-HEADING (1).              ELTWAITP
00731      MOVE +1                     TO  WS-HEADINGS-CNT.             ELTWAITP
00732      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00733                                                                   ELTWAITP
00734                                                                   ELTWAITP
00735 ************************************************************      ELTWAITP
00736 *                                                          *      ELTWAITP
00737 *        SETUP BS EXPENSE FREE SENTENCE                    *      ELTWAITP
00738 *                                                          *      ELTWAITP
00739 ************************************************************      ELTWAITP
00740  SETUP-BS-EXPENSE-FREE-SENTENCE.                                  ELTWAITP
00741      MOVE GCG-BS-EXPENSE-FREE-IND  TO  CMF-CODE-VALUE.            ELTWAITP
00742      MOVE 'BS-EXPENSE-FREE-IND'    TO  CMF-ELEMENT-SYSTEM-NAME.   ELTWAITP
00743      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00744      MOVE GCG-BS-EXPENSE-FREE-DAYS TO EF-IND-DAYS.                ELTWAITP
00745      MOVE EXP-FREE-DAYS            TO WS-TEXT (1).                ELTWAITP
00746      MOVE +1                       TO WS-LINE-CNT.                ELTWAITP
00747      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
00748      MOVE EXP-FREE-IND-SENT-1      TO WS-HEADING (1).             ELTWAITP
00749      MOVE EXP-FREE-IND-SENT-2      TO WS-HEADING (2).             ELTWAITP
00750      MOVE +2                       TO WS-HEADINGS-CNT.            ELTWAITP
00751      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00752                                                                   ELTWAITP
00753                                                                   ELTWAITP
00754 ************************************************************      ELTWAITP
00755 *                                                          *      ELTWAITP
00756 *        SETUP BS OB WAIT PERIOD SENTENCE                  *      ELTWAITP
00757 *                                                          *      ELTWAITP
00758 ************************************************************      ELTWAITP
00759  SETUP-BS-OB-WAIT-PERIOD-SENTEN.                                  ELTWAITP
00760      MOVE GCG-BS-OB-WAITG-PERD-IND  TO  CMF-CODE-VALUE.           ELTWAITP
00761      MOVE 'BS-OB-WAITG-PERD-IND'    TO  CMF-ELEMENT-SYSTEM-NAME.  ELTWAITP
00762      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00763      MOVE WAIT-PERD-OB-SENT-1       TO  WS-HEADING (1).           ELTWAITP
00764      MOVE WAIT-PERD-OB-SENT-2       TO  WS-HEADING (2).           ELTWAITP
00765      MOVE +2                        TO  WS-HEADINGS-CNT.          ELTWAITP
00766      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00767      MOVE WAIT-PERD-OB-DAYS-SENT-1  TO  WS-HEADING (1).           ELTWAITP
00768      MOVE PC-DAYS                   TO  WS-HEADING (2).           ELTWAITP
00769      MOVE +2                        TO  WS-HEADINGS-CNT.          ELTWAITP
00770      MOVE ZEROS  TO  WP-OB-DAYS-MBR,                              ELTWAITP
00771                      WP-OB-DAYS-SPS,                              ELTWAITP
00772                      WP-OB-DAYS-DEP.                              ELTWAITP
00773      IF GCG-BS-OB-WAITG-PERD-MEM-DAYS NUMERIC AND                 ELTWAITP
00774                  MEMBER-FAM-REL                                   ELTWAITP
00775          PERFORM MOVE-MBR-BS-OB-WAITING-DAYS.                     ELTWAITP
00776      IF GCG-BS-OB-WAITG-PERD-SPS-DAYS NUMERIC AND                 ELTWAITP
00777                  SPOUSE-FAM-REL                                   ELTWAITP
00778          PERFORM MOVE-SPS-BS-OB-WAITING-DAYS.                     ELTWAITP
00779      IF GCG-BS-OB-WAITG-PERD-DEP-DAYS NUMERIC AND                 ELTWAITP
00780                  DEPENDENT-FAM-REL                                ELTWAITP
00781          PERFORM MOVE-DEP-BS-OB-WAITING-DAYS.                     ELTWAITP
00782      PERFORM DISPLAY-INFORMATION-STORED-INX.                      ELTWAITP
00783                                                                   ELTWAITP
00784                                                                   ELTWAITP
00785 ************************************************************      ELTWAITP
00786 *                                                          *      ELTWAITP
00787 *        MOVE MBR BS OB WAITING DAYS                       *      ELTWAITP
00788 *                                                          *      ELTWAITP
00789 ************************************************************      ELTWAITP
00790  MOVE-MBR-BS-OB-WAITING-DAYS.                                     ELTWAITP
00791      MOVE GCG-BS-OB-WAITG-PERD-MEM-DAYS                           ELTWAITP
00792                TO  WP-OB-DAYS-MBR.                                ELTWAITP
00793      ADD  +1   TO  WS-LINE-CNT.                                   ELTWAITP
00794      MOVE WAIT-PERD-OB-DAYS-MBR                                   ELTWAITP
00795                TO  WS-TEXT (WS-OUTPUT-IDX).                       ELTWAITP
00796      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
00797                                                                   ELTWAITP
00798                                                                   ELTWAITP
00799 ************************************************************      ELTWAITP
00800 *                                                          *      ELTWAITP
00801 *        MOVE SPS BS OB WAITING DAYS                       *      ELTWAITP
00802 *                                                          *      ELTWAITP
00803 ************************************************************      ELTWAITP
00804  MOVE-SPS-BS-OB-WAITING-DAYS.                                     ELTWAITP
00805      MOVE GCG-BS-OB-WAITG-PERD-SPS-DAYS                           ELTWAITP
00806                TO  WP-OB-DAYS-SPS.                                ELTWAITP
00807      ADD  +1   TO  WS-LINE-CNT.                                   ELTWAITP
00808      MOVE WAIT-PERD-OB-DAYS-SPS                                   ELTWAITP
00809                TO  WS-TEXT (WS-OUTPUT-IDX).                       ELTWAITP
00810      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
00811                                                                   ELTWAITP
00812                                                                   ELTWAITP
00813 ************************************************************      ELTWAITP
00814 *                                                          *      ELTWAITP
00815 *        MOVE DEP BS OB WAITING DAYS                       *      ELTWAITP
00816 *                                                          *      ELTWAITP
00817 ************************************************************      ELTWAITP
00818  MOVE-DEP-BS-OB-WAITING-DAYS.                                     ELTWAITP
00819      MOVE GCG-BS-OB-WAITG-PERD-DEP-DAYS                           ELTWAITP
00820                TO  WP-OB-DAYS-DEP.                                ELTWAITP
00821      ADD  +1   TO  WS-LINE-CNT.                                   ELTWAITP
00822      MOVE WAIT-PERD-OB-DAYS-DEP                                   ELTWAITP
00823                TO  WS-TEXT (WS-OUTPUT-IDX).                       ELTWAITP
00824      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
00825 /***********************************************************      ELTWAITP
00826 *                                                          *      ELTWAITP
00827 *        PROCESS MM SCREEN                                 *      ELTWAITP
00828 *                                                          *      ELTWAITP
00829 ************************************************************      ELTWAITP
00830  PROCESS-MM-SCREEN.                                               ELTWAITP
00831      IF GCG-MM-WAITG-PERD-IND = ZERO OR SPACE                     ELTWAITP
00832          PERFORM SETUP-NOT-APPLICABLE-MESSAGE                     ELTWAITP
00833      ELSE                                                         ELTWAITP
00834          PERFORM SETUP-MM-SENTENCES.                              ELTWAITP
00835      PERFORM DISPLAY-DASHED-LINE-AT-END-OFX.                      ELTWAITP
00836                                                                   ELTWAITP
00837                                                                   ELTWAITP
00838 ************************************************************      ELTWAITP
00839 *                                                          *      ELTWAITP
00840 *        SETUP MM SENTENCES                                *      ELTWAITP
00841 *                                                          *      ELTWAITP
00842 ************************************************************      ELTWAITP
00843  SETUP-MM-SENTENCES.                                              ELTWAITP
00844      PERFORM SETUP-MM-WAIT-PERIOD-IND-SENTE.                      ELTWAITP
00845      PERFORM SETUP-MM-WAIT-PERIOD-DAYS-SENT.                      ELTWAITP
00846      IF GCG-MM-WAIVR-IND NOT = ZERO AND SPACE                     ELTWAITP
00847          PERFORM SETUP-MM-WAIVER-SENTENCE.                        ELTWAITP
00848      IF GCG-MM-EXPENSE-FREE-IND NOT = ZERO AND SPACE              ELTWAITP
00849          PERFORM SETUP-MM-EXPENSE-FREE-SENTENCE.                  ELTWAITP
00850      IF GCG-MM-OB-WAITG-PERD-IND NOT = ZERO AND SPACE             ELTWAITP
00851          PERFORM SETUP-MM-OB-WAIT-PERIOD-SENTEN.                  ELTWAITP
00852                                                                   ELTWAITP
00853                                                                   ELTWAITP
00854 ************************************************************      ELTWAITP
00855 *                                                          *      ELTWAITP
00856 *        SETUP MM WAIT PERIOD IND SENTENCE                 *      ELTWAITP
00857 *                                                          *      ELTWAITP
00858 ************************************************************      ELTWAITP
00859  SETUP-MM-WAIT-PERIOD-IND-SENTE.                                  ELTWAITP
00860      MOVE GCG-MM-WAITG-PERD-IND  TO  CMF-CODE-VALUE.              ELTWAITP
00861      MOVE 'MM-WAITG-PERD-IND'    TO                               ELTWAITP
00862          CMF-ELEMENT-SYSTEM-NAME.                                 ELTWAITP
00863      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00864      MOVE WAIT-PERD-IND-SENT-1   TO  WS-HEADING (1).              ELTWAITP
00865      MOVE WAIT-PERD-IND-SENT-2   TO  WS-HEADING (2).              ELTWAITP
00866      MOVE +2                     TO  WS-HEADINGS-CNT.             ELTWAITP
00867      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00868                                                                   ELTWAITP
00869                                                                   ELTWAITP
00870 ************************************************************      ELTWAITP
00871 *                                                          *      ELTWAITP
00872 *        SETUP MM WAIT PERIOD DAYS SENTENCE                *      ELTWAITP
00873 *                                                          *      ELTWAITP
00874 ************************************************************      ELTWAITP
00875  SETUP-MM-WAIT-PERIOD-DAYS-SENT.                                  ELTWAITP
00876      MOVE ZEROS  TO  WP-DAYS-MBR,                                 ELTWAITP
00877                      WP-DAYS-SPS,                                 ELTWAITP
00878                      WP-DAYS-DEP.                                 ELTWAITP
00879      IF GCG-MM-WAITG-PERD-MEM-DAYS NUMERIC                        ELTWAITP
00880          MOVE GCG-MM-WAITG-PERD-MEM-DAYS  TO  WP-DAYS-MBR.        ELTWAITP
00881      IF GCG-MM-WAITG-PERD-SPS-DAYS NUMERIC                        ELTWAITP
00882          MOVE GCG-MM-WAITG-PERD-SPS-DAYS  TO  WP-DAYS-SPS.        ELTWAITP
00883      IF GCG-MM-WAITG-PERD-DEP-DAYS NUMERIC                        ELTWAITP
00884          MOVE GCG-MM-WAITG-PERD-DEP-DAYS  TO  WP-DAYS-DEP.        ELTWAITP
00885      MOVE WAIT-PERD-DAYS-SENT-1  TO  WS-HEADING (1).              ELTWAITP
00886      MOVE PC-DAYS                TO  WS-HEADING (2).              ELTWAITP
00887      MOVE +2                     TO  WS-HEADINGS-CNT.             ELTWAITP
00888      IF MEMBER-FAM-REL                                            ELTWAITP
00889          PERFORM SHOW-MEMBER-WAIT-DAYS.                           ELTWAITP
00890      IF SPOUSE-FAM-REL                                            ELTWAITP
00891          PERFORM SHOW-SPOUSE-WAIT-DAYS.                           ELTWAITP
00892      IF DEPENDENT-FAM-REL                                         ELTWAITP
00893          PERFORM SHOW-DEPENDENT-WAIT-DAYS.                        ELTWAITP
00894      PERFORM DISPLAY-INFORMATION-STORED-INX.                      ELTWAITP
00895                                                                   ELTWAITP
00896                                                                   ELTWAITP
00897 ************************************************************      ELTWAITP
00898 *                                                          *      ELTWAITP
00899 *        SETUP MM WAIVER SENTENCE                          *      ELTWAITP
00900 *                                                          *      ELTWAITP
00901 ************************************************************      ELTWAITP
00902  SETUP-MM-WAIVER-SENTENCE.                                        ELTWAITP
00903      MOVE GCG-MM-WAIVR-IND   TO  CMF-CODE-VALUE.                  ELTWAITP
00904      MOVE 'MM-WAIVR-IND'     TO                                   ELTWAITP
00905          CMF-ELEMENT-SYSTEM-NAME.                                 ELTWAITP
00906      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00907      MOVE WAIT-PERD-WAIVER-SENT  TO WS-HEADING (1).               ELTWAITP
00908      MOVE +1                     TO WS-HEADINGS-CNT.              ELTWAITP
00909      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00910                                                                   ELTWAITP
00911                                                                   ELTWAITP
00912 ************************************************************      ELTWAITP
00913 *                                                          *      ELTWAITP
00914 *        SETUP MM EXPENSE FREE SENTENCE                    *      ELTWAITP
00915 *                                                          *      ELTWAITP
00916 ************************************************************      ELTWAITP
00917  SETUP-MM-EXPENSE-FREE-SENTENCE.                                  ELTWAITP
00918      MOVE GCG-MM-EXPENSE-FREE-IND  TO  CMF-CODE-VALUE.            ELTWAITP
00919      MOVE 'MM-EXPENSE-FREE-IND'    TO  CMF-ELEMENT-SYSTEM-NAME.   ELTWAITP
00920      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00921      MOVE GCG-MM-EXPENSE-FREE-DAYS TO EF-IND-DAYS.                ELTWAITP
00922      MOVE EXP-FREE-DAYS            TO WS-TEXT (1).                ELTWAITP
00923      MOVE +1                       TO WS-LINE-CNT.                ELTWAITP
00924      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
00925      MOVE EXP-FREE-IND-SENT-1      TO WS-HEADING (1).             ELTWAITP
00926      MOVE EXP-FREE-IND-SENT-2      TO WS-HEADING (2).             ELTWAITP
00927      MOVE +2                       TO WS-HEADINGS-CNT.            ELTWAITP
00928      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00929                                                                   ELTWAITP
00930                                                                   ELTWAITP
00931 ************************************************************      ELTWAITP
00932 *                                                          *      ELTWAITP
00933 *        SETUP MM OB WAIT PERIOD SENTENCE                  *      ELTWAITP
00934 *                                                          *      ELTWAITP
00935 ************************************************************      ELTWAITP
00936  SETUP-MM-OB-WAIT-PERIOD-SENTEN.                                  ELTWAITP
00937      MOVE GCG-MM-OB-WAITG-PERD-IND  TO  CMF-CODE-VALUE.           ELTWAITP
00938      MOVE 'MM-OB-WAITG-PERD-IND'    TO  CMF-ELEMENT-SYSTEM-NAME.  ELTWAITP
00939      PERFORM LINK-TO-TRANSLATOR-MODULE.                           ELTWAITP
00940      MOVE WAIT-PERD-OB-SENT-1       TO WS-HEADING (1).            ELTWAITP
00941      MOVE WAIT-PERD-OB-SENT-2       TO WS-HEADING (2).            ELTWAITP
00942      MOVE +2                        TO WS-HEADINGS-CNT.           ELTWAITP
00943      PERFORM FORMAT-SENTENCE.                                     ELTWAITP
00944      MOVE WAIT-PERD-OB-DAYS-SENT-1  TO WS-HEADING (1).            ELTWAITP
00945      MOVE PC-DAYS                   TO WS-HEADING (2).            ELTWAITP
00946      MOVE +2                        TO WS-HEADINGS-CNT.           ELTWAITP
00947      MOVE ZEROS  TO  WP-OB-DAYS-MBR,                              ELTWAITP
00948                      WP-OB-DAYS-SPS,                              ELTWAITP
00949                      WP-OB-DAYS-DEP.                              ELTWAITP
00950      IF GCG-MM-OB-WAITG-PERD-MEM-DAYS NUMERIC AND                 ELTWAITP
00951              MEMBER-FAM-REL                                       ELTWAITP
00952          PERFORM MOVE-MBR-MM-OB-WAITING-DAYS.                     ELTWAITP
00953      IF GCG-MM-OB-WAITG-PERD-SPS-DAYS NUMERIC AND                 ELTWAITP
00954              SPOUSE-FAM-REL                                       ELTWAITP
00955          PERFORM MOVE-SPS-MM-OB-WAITING-DAYS.                     ELTWAITP
00956      IF GCG-MM-OB-WAITG-PERD-DEP-DAYS NUMERIC AND                 ELTWAITP
00957              DEPENDENT-FAM-REL                                    ELTWAITP
00958          PERFORM MOVE-DEP-MM-OB-WAITING-DAYS.                     ELTWAITP
00959      PERFORM DISPLAY-INFORMATION-STORED-INX.                      ELTWAITP
00960                                                                   ELTWAITP
00961                                                                   ELTWAITP
00962 ************************************************************      ELTWAITP
00963 *                                                          *      ELTWAITP
00964 *        MOVE MBR MM OB WAITING DAYS                       *      ELTWAITP
00965 *                                                          *      ELTWAITP
00966 ************************************************************      ELTWAITP
00967  MOVE-MBR-MM-OB-WAITING-DAYS.                                     ELTWAITP
00968      MOVE GCG-MM-OB-WAITG-PERD-MEM-DAYS                           ELTWAITP
00969                TO  WP-OB-DAYS-MBR.                                ELTWAITP
00970      ADD  +1   TO  WS-LINE-CNT.                                   ELTWAITP
00971      MOVE WAIT-PERD-OB-DAYS-MBR                                   ELTWAITP
00972                TO  WS-TEXT (WS-OUTPUT-IDX).                       ELTWAITP
00973      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
00974                                                                   ELTWAITP
00975                                                                   ELTWAITP
00976 ************************************************************      ELTWAITP
00977 *                                                          *      ELTWAITP
00978 *        MOVE SPS MM OB WAITING DAYS                       *      ELTWAITP
00979 *                                                          *      ELTWAITP
00980 ************************************************************      ELTWAITP
00981  MOVE-SPS-MM-OB-WAITING-DAYS.                                     ELTWAITP
00982      MOVE GCG-MM-OB-WAITG-PERD-SPS-DAYS                           ELTWAITP
00983                TO  WP-OB-DAYS-SPS.                                ELTWAITP
00984      ADD  +1   TO  WS-LINE-CNT.                                   ELTWAITP
00985      MOVE WAIT-PERD-OB-DAYS-SPS                                   ELTWAITP
00986                TO  WS-TEXT (WS-OUTPUT-IDX).                       ELTWAITP
00987      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
00988                                                                   ELTWAITP
00989                                                                   ELTWAITP
00990 ************************************************************      ELTWAITP
00991 *                                                          *      ELTWAITP
00992 *        MOVE DEP MM OB WAITING DAYS                       *      ELTWAITP
00993 *                                                          *      ELTWAITP
00994 ************************************************************      ELTWAITP
00995  MOVE-DEP-MM-OB-WAITING-DAYS.                                     ELTWAITP
00996      MOVE GCG-MM-OB-WAITG-PERD-DEP-DAYS                           ELTWAITP
00997                TO  WP-OB-DAYS-DEP.                                ELTWAITP
00998      ADD  +1   TO  WS-LINE-CNT.                                   ELTWAITP
00999      MOVE WAIT-PERD-OB-DAYS-DEP                                   ELTWAITP
01000                TO  WS-TEXT (WS-OUTPUT-IDX).                       ELTWAITP
01001      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
01002 /***********************************************************      ELTWAITP
01003 *                                                          *      ELTWAITP
01004 *        SHOW MEMBER WAIT DAYS                             *      ELTWAITP
01005 *                                                          *      ELTWAITP
01006 ************************************************************      ELTWAITP
01007  SHOW-MEMBER-WAIT-DAYS.                                           ELTWAITP
01008      ADD  +1                  TO  WS-LINE-CNT.                    ELTWAITP
01009      MOVE WAIT-PERD-DAYS-MBR  TO  WS-TEXT                         ELTWAITP
01010          (WS-OUTPUT-IDX).                                         ELTWAITP
01011      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
01012                                                                   ELTWAITP
01013                                                                   ELTWAITP
01014 ************************************************************      ELTWAITP
01015 *                                                          *      ELTWAITP
01016 *        SHOW SPOUSE WAIT DAYS                             *      ELTWAITP
01017 *                                                          *      ELTWAITP
01018 ************************************************************      ELTWAITP
01019  SHOW-SPOUSE-WAIT-DAYS.                                           ELTWAITP
01020      ADD  +1                  TO  WS-LINE-CNT.                    ELTWAITP
01021      MOVE WAIT-PERD-DAYS-SPS  TO  WS-TEXT (WS-OUTPUT-IDX).        ELTWAITP
01022      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
01023                                                                   ELTWAITP
01024                                                                   ELTWAITP
01025 ************************************************************      ELTWAITP
01026 *                                                          *      ELTWAITP
01027 *        SHOW DEPENDENT WAIT DAYS                          *      ELTWAITP
01028 *                                                          *      ELTWAITP
01029 ************************************************************      ELTWAITP
01030  SHOW-DEPENDENT-WAIT-DAYS.                                        ELTWAITP
01031      ADD  +1                  TO  WS-LINE-CNT.                    ELTWAITP
01032      MOVE WAIT-PERD-DAYS-DEP  TO  WS-TEXT (WS-OUTPUT-IDX).        ELTWAITP
01033      SET  WS-OUTPUT-IDX UP BY +1.                                 ELTWAITP
01034                                                                   ELTWAITP
01035                                                                   ELTWAITP
01036 ************************************************************      ELTWAITP
01037 *                                                          *      ELTWAITP
01038 *        SETUP NOT APPLICABLE MESSAGE                      *      ELTWAITP
01039 *                                                          *      ELTWAITP
01040 ************************************************************      ELTWAITP
01041  SETUP-NOT-APPLICABLE-MESSAGE.                                    ELTWAITP
01042      MOVE PC-NOT-APPLICABLE      TO  WS-TEXT (1).                 ELTWAITP
01043      MOVE WAIT-PERD-IND-SENT-1   TO  WS-HEADING (1).              ELTWAITP
01044      MOVE WAIT-PERD-IND-SENT-2   TO  WS-HEADING (2).              ELTWAITP
01045      MOVE +2                     TO  WS-HEADINGS-CNT.             ELTWAITP
01046      MOVE +2                     TO  WS-LINE-CNT.                 ELTWAITP
01047      PERFORM DISPLAY-INFORMATION-STORED-INX.                      ELTWAITP
01048 /***********************************************************      ELTWAITP
01049 *                                                          *      ELTWAITP
01050 *        DISPLAY DASHED LINE AT END OF TEXT                *      ELTWAITP
01051 *                                                          *      ELTWAITP
01052 ************************************************************      ELTWAITP
01053  DISPLAY-DASHED-LINE-AT-END-OFX.                                  ELTWAITP
01054      MOVE +1                 TO COF-NBR-DTL-LINES.                ELTWAITP
01055      MOVE ALL '-'            TO COF-DTL-LINE (1).                 ELTWAITP
01056      PERFORM LINK-TO-OUTPUT-MODULE.                               ELTWAITP
01057                                                                   ELTWAITP
01058                                                                   ELTWAITP
01059 ************************************************************      ELTWAITP
01060 *                                                          *      ELTWAITP
01061 *        LINK TO TRANSLATOR MODULE                         *      ELTWAITP
01062 *                                                          *      ELTWAITP
01063 ************************************************************      ELTWAITP
01064  LINK-TO-TRANSLATOR-MODULE.                                       ELTWAITP
01065      EXEC CICS LINK PROGRAM ('ELUCMIF')                           ELTWAITP
01066                COMMAREA (DFHCOMMAREA)                             ELTWAITP
01067                END-EXEC.                                          ELTWAITP
01068      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTWAITP
01069      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTWAITP
01070                 ADDRESS OF CMF-DESCR.                             ELTWAITP
01071                                                                   ELTWAITP
01072                                                                   ELTWAITP
01073 ************************************************************      ELTWAITP
01074 *                                                          *      ELTWAITP
01075 *        LINK TO OUTPUT MODULE                             *      ELTWAITP
01076 *                                                          *      ELTWAITP
01077 ************************************************************      ELTWAITP
01078  LINK-TO-OUTPUT-MODULE.                                           ELTWAITP
01079      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTWAITP
01080                     COMMAREA (DFHCOMMAREA)                        ELTWAITP
01081                     END-EXEC.                                     ELTWAITP
01082 /***********************************************************      ELTWAITP
01083 *                                                          *      ELTWAITP
01084 *        FORMAT SENTENCE                                   *      ELTWAITP
01085 *                                                          *      ELTWAITP
01086 ************************************************************      ELTWAITP
01087  FORMAT-SENTENCE.                                                 ELTWAITP
01088      PERFORM INITIALIZE-TEXT-COMPRESSION-AR.                      ELTWAITP
01089      PERFORM  VARYING CMF-DESCR-IDX FROM 1 BY 1                   ELTWAITP
01090                   UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES       ELTWAITP
01091          ADD +1  TO  TCAR-FROM-SUB                                ELTWAITP
01092          MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                   ELTWAITP
01093               TCAR-FROM-LINE (TCAR-FROM-SUB)                      ELTWAITP
01094      END-PERFORM.                                                 ELTWAITP
01095      IF INSERT-PERIOD                                             ELTWAITP
01096          ADD  +1      TO TCAR-FROM-SUB                            ELTWAITP
01097          MOVE '.'     TO TCAR-FROM-LINE-LAST-DIGIT                ELTWAITP
01098              (TCAR-FROM-SUB)                                      ELTWAITP
01099      END-IF.                                                      ELTWAITP
01100      PERFORM FORMAT-THE-RAW-TEXT.                                 ELTWAITP
01101      PERFORM MOVE-TEXT-TO-DISPLAY.                                ELTWAITP
01102      PERFORM DISPLAY-INFORMATION-STORED-INX.                      ELTWAITP
01103                                                                   ELTWAITP
01104                                                                   ELTWAITP
01105 ************************************************************      ELTWAITP
01106 *                                                          *      ELTWAITP
01107 *        INITIALIZE TEXT COMPRESSION AREA                  *      ELTWAITP
01108 *                                                          *      ELTWAITP
01109 ************************************************************      ELTWAITP
01110  INITIALIZE-TEXT-COMPRESSION-AR.                                  ELTWAITP
01111      INITIALIZE TCAR-FROM-SUB                                     ELTWAITP
01112                 TCAR-FROM-AREA                                    ELTWAITP
01113                 TCAR-FROM-LENGTH                                  ELTWAITP
01114                 TCAR-X.                                           ELTWAITP
01115                                                                   ELTWAITP
01116                                                                   ELTWAITP
01117 ************************************************************      ELTWAITP
01118 *                                                          *      ELTWAITP
01119 *        FORMAT THE RAW TEXT                               *      ELTWAITP
01120 *                                                          *      ELTWAITP
01121 ************************************************************      ELTWAITP
01122  FORMAT-THE-RAW-TEXT.                                             ELTWAITP
01123      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTWAITP
01124      MOVE +10 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTWAITP
01125      MOVE +46 TO TCAR-OUTPUT-FIELD-1-LEN                          ELTWAITP
01126                  TCAR-OUTPUT-FIELD-2-LEN                          ELTWAITP
01127                  TCAR-OUTPUT-FIELD-3-LEN                          ELTWAITP
01128                  TCAR-OUTPUT-FIELD-4-LEN                          ELTWAITP
01129                  TCAR-OUTPUT-FIELD-5-LEN                          ELTWAITP
01130                  TCAR-OUTPUT-FIELD-6-LEN                          ELTWAITP
01131                  TCAR-OUTPUT-FIELD-7-LEN                          ELTWAITP
01132                  TCAR-OUTPUT-FIELD-8-LEN                          ELTWAITP
01133                  TCAR-OUTPUT-FIELD-9-LEN                          ELTWAITP
01134                  TCAR-OUTPUT-FIELD-10-LEN.                        ELTWAITP
01135      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTWAITP
01136 /***********************************************************      ELTWAITP
01137 *                                                          *      ELTWAITP
01138 *        MOVE TEXT TO DISPLAY                              *      ELTWAITP
01139 *                                                          *      ELTWAITP
01140 ************************************************************      ELTWAITP
01141  MOVE-TEXT-TO-DISPLAY.                                            ELTWAITP
01142      PERFORM                                                      ELTWAITP
01143          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTWAITP
01144                UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED OR   ELTWAITP
01145                      TCAR-FROM-SUB > 20                           ELTWAITP
01146          ADD 1                        TO WS-LINE-CNT              ELTWAITP
01147          MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO                    ELTWAITP
01148               WS-TEXT (WS-OUTPUT-IDX)                             ELTWAITP
01149          SET  WS-OUTPUT-IDX UP BY +1                              ELTWAITP
01150      END-PERFORM.                                                 ELTWAITP
01151                                                                   ELTWAITP
01152                                                                   ELTWAITP
01153 ************************************************************      ELTWAITP
01154 *                                                          *      ELTWAITP
01155 *        DISPLAY INFORMATION STORED IN WS OUTPUT AREA      *      ELTWAITP
01156 *                                                          *      ELTWAITP
01157 ************************************************************      ELTWAITP
01158  DISPLAY-INFORMATION-STORED-INX.                                  ELTWAITP
01159      IF WS-LINE-CNT < WS-HEADINGS-CNT                             ELTWAITP
01160          ADD 1 TO WS-LINE-CNT.                                    ELTWAITP
01161      PERFORM                                                      ELTWAITP
01162          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTWAITP
01163                  UNTIL   WS-OUTPUT-IDX > WS-LINE-CNT + 1          ELTWAITP
01164          ADD  +1        TO COF-NBR-DTL-LINES                      ELTWAITP
01165          MOVE WS-BAR    TO WS-DIVIDER (WS-OUTPUT-IDX)             ELTWAITP
01166          MOVE WS-OUTPUT-LINE (WS-OUTPUT-IDX)                      ELTWAITP
01167            TO COF-DTL-LINE (COF-NBR-DTL-LINES)                    ELTWAITP
01168      END-PERFORM.                                                 ELTWAITP
01169      PERFORM LINK-TO-OUTPUT-MODULE.                               ELTWAITP
01170      INITIALIZE WS-HEADINGS-CNT                                   ELTWAITP
01171                 WS-LINE-CNT                                       ELTWAITP
01172                 WS-OUTPUT.                                        ELTWAITP
01173      SET  WS-OUTPUT-IDX TO +1.                                    ELTWAITP
01174 /***********************************************************      ELTWAITP
01175 *                                                          *      ELTWAITP
01176 *        SIGNAL UNALLOC AREA ERROR                         *      ELTWAITP
01177 *                                                          *      ELTWAITP
01178 ************************************************************      ELTWAITP
01179  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTWAITP
01180      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTWAITP
01181      PERFORM SIGNAL-ABEND.                                        ELTWAITP
01182                                                                   ELTWAITP
01183                                                                   ELTWAITP
01184 ************************************************************      ELTWAITP
01185 *                                                          *      ELTWAITP
01186 *        SIGNAL ABEND                                      *      ELTWAITP
01187 *                                                          *      ELTWAITP
01188 ************************************************************      ELTWAITP
01189  SIGNAL-ABEND.                                                    ELTWAITP
01190      EXEC CICS ABEND                                              ELTWAITP
01191                ABCODE(CIA-ABCODE)                                 ELTWAITP
01192         END-EXEC.                                                 ELTWAITP
01193 /                                                                 ELTWAITP
01194      COPY ELSTCOMP.                                               ELTWAITP
