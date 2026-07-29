00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSSLCCP
00003  PROGRAM-ID.         ELSSLCCP.                                       LV002
00004                                                                   ELSSLCCP
00005  AUTHOR.             EDWARD G LISS                                ELSSLCCP
00006                      (CLONED FROM ELSSUBTP)                       ELSSLCCP
00007                                                                   ELSSLCCP
00008  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSSLCCP
00009                      A MUTUAL LEGAL RESERVE COMPANY               ELSSLCCP
00010                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSSLCCP
00011                      233 N. MICHIGAN AVE                          ELSSLCCP
00012                      CHICAGO, ILLINOIS 60601                      ELSSLCCP
00013                                                                   ELSSLCCP
00014  DATE-WRITTEN.       05-MAR-1987.                                 ELSSLCCP
00015                                                                   ELSSLCCP
00016  DATE-COMPILED.                                                   ELSSLCCP
00017                                                                   ELSSLCCP
00018  SECURITY.           COPYRIGHT 1987,                              ELSSLCCP
00019                      HEALTH CARE SERVICE CORPORATION              ELSSLCCP
00020      SKIP3                                                        ELSSLCCP
00021 ******************************************************************ELSSLCCP
00022 *                                                                *ELSSLCCP
00023 *    PROGRAM:    ELSSLCCP                                        *ELSSLCCP
00024 *    DATE:       05-MAR-1987                                     *ELSSLCCP
00025 *    AUTHOR:     EDWARD G LISS                                   *ELSSLCCP
00026 *    FUNCTION:   COST CONTAINMENT SELECTION PROGRAM              *ELSSLCCP
00027 *                                                                *ELSSLCCP
00028 ******************************************************************ELSSLCCP
00029 *                                                                *ELSSLCCP
00030 *                      MAINTENANCE HISTORY                       *ELSSLCCP
00031 *                                                                *ELSSLCCP
00032 *  MOD     DATE     BY  DRPT                ACTION               *ELSSLCCP
00033 * ----- ----------- --- ----- ---------------------------------- *ELSSLCCP
00034 * 01.00 05-MAR-1987 EGL       CREATED                            *ELSSLCCP
00035 *                                                                *ELSSLCCP
00036 * 01.01 22-SEP-1987 REB       ISSUE ABEND CODES OF 'EL01' IF AN  *ELSSLCCP
00037 *                             INVALID COMMAREA AND 'EL02' IF THE *ELSSLCCP
00038 *                             CIA BLOCK IS NOT PRESENT.          *ELSSLCCP
00039 *                                                                *ELSSLCCP
00040 * 01.02 07-APR-1988 AKK       CHANGED PARTICIPATING TO PREFERRED *ELSSLCCP
00041 *                                                                *ELSSLCCP
00042 * 01.03 11-APR-1989 EGL       CONVERTED FROM STRUCTURES          *ELSSLCCP
00043 *                                                                *ELSSLCCP
00044 * 01.04 13-APR-1989 GEM       STORAGE MANAGAMENT ENHANCEMENTS    *ELSSLCCP
00045 *                                                                *ELSSLCCP
00046 * 01.05 13-NOV-1989 AKK       ISSR #10945 CHANGES TO MENU THAT   *ELSSLCCP
00047 *                             WILL ELIMINATE THOSE CCPS THAT     *ELSSLCCP
00048 *                             ARE NOT APPLICABLE AND ONLY LIST   *ELSSLCCP
00049 *                             THAT ARE.                          *ELSSLCCP
00050 *                                                                *ELSSLCCP
00051 * 01.06 15-AUG-1990 AKK       ADDED MANAGED CARE NETWORK AND     *ELSSLCCP
00052 *                             SUBSTANCE ABUSE/MENTAL TOPICS      *ELSSLCCP
00053 *                                                                *ELSSLCCP
00054 * 01.07 15-MAY-1991 AKK       ADDED POINT OF SERVICE AND         *ELSSLCCP
00055 *                             MENTAL HEALTH SUBSTANCE ABUSE      *ELSSLCCP
00056 *                             TOPICS. ISSR #11836.               *ELSSLCCP
00057 *                             ALSO CHANGED REFERENCE TO SAM TO   *ELSSLCCP
00058 *                             EXTENDED MENTAL HEALTH DUE TO      *ELSSLCCP
00059 *                             PROGRAM NAME CHANGE.               *ELSSLCCP
00060 *                                                                *ELSSLCCP
00061 * 01.08 23-MAY-1991 AKK       REMOVED VOLUNTARY CHECKS BECAUSE   *ELSSLCCP
00062 *                             ELS IS NOW TRANSLATING THE         *ELSSLCCP
00063 *                             GROUP SPEC PARTICPATION INDICATOR  *ELSSLCCP
00064 *                             WITHIN EACH TOPIC.                 *ELSSLCCP
00065 *                                                                *ELSSLCCP
00066 * 01.09 04-JAN-1993 AKK       ADDED RESTRICTED PROVIDER OPTION   *ELSSLCCP
00067 *                             TOPIC.                             *ELSSLCCP
00068 *                                                                *ELSSLCCP
00069 * 01.10 20-FEB-1995 AKK       ADDED COMMUNITY PROVIDER OPTION    *ELSSLCCP
00070 *                             TOPIC.                             *ELSSLCCP
00071 *                                                                *ELSSLCCP
00072 * 01.11 19-FEB-1996 AKK       ADDED COMMUNITY BLUE AND PREFERRED *ELSSLCCP
00073 *                             ANCILLARY NETWORK.                 *ELSSLCCP
00074 *                                                                *ELSSLCCP
00075 * 01.12 20-OCT-1997 AKK       ADDED SUPPORT FOR YR2000 AND       *ELSSLCCP
00076 *                             TEXAS MERGE.                       *ELSSLCCP
00077 * 01.13 11-MAR-1999 AKK       ADDED BAE BLUE ADVANTAGE ENTR.     *ELSSLCCP
00078 *                             TOPIC                               ELSSLCCP
00079 * 01.14 06-MAR-2001 AKK       ADDED SUPPORT FOR HMO MANAGED CARE *ELSSLCCP
00080 *                             TOPIC FOR NEW MEXICO.               ELSSLCCP
00081 *       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    *ELSSLCCP
00082 ******************************************************************ELSSLCCP
00083      EJECT                                                        ELSSLCCP
00084  ENVIRONMENT DIVISION.                                            ELSSLCCP
00085                                                                   ELSSLCCP
00086  CONFIGURATION SECTION.                                           ELSSLCCP
00087  SOURCE-COMPUTER.    IBM-3033.                                    ELSSLCCP
00088  OBJECT-COMPUTER.    IBM-3033.                                    ELSSLCCP
00089      EJECT                                                        ELSSLCCP
00090  DATA DIVISION.                                                   ELSSLCCP
00091                                                                   ELSSLCCP
00092  FILE SECTION.                                                    ELSSLCCP
00093                                                                   ELSSLCCP
00094  WORKING-STORAGE SECTION.                                         ELSSLCCP
00095  01  MISC-WS.                                                     ELSSLCCP
00096      05  MOVE-SUB            PIC S9(4) COMP.                      ELSSLCCP
00097      05  WS-COST-MNU-TITLE   PIC X(16) VALUE 'COST CONTAINMENT'.  ELSSLCCP
00098      05  WS-COST-PROGRAM-TITLE PIC X(25) VALUE                    ELSSLCCP
00099             'COST CONTAINMENT PROGRAMS'.                          ELSSLCCP
00100      05  WS-FOUND-SW         PIC X  VALUE 'N'.                    ELSSLCCP
00101          88  WS-GROUP-FOUND         VALUE 'Y'.                    ELSSLCCP
00102          88  WS-GROUP-NOT-FOUND     VALUE 'N'.                    ELSSLCCP
00103      05  WS-APPLICABLE-CTR   PIC S9(4) COMP  VALUE 0.             ELSSLCCP
00104      05  WS-APPLICABLE-NUM   PIC 99          VALUE 0.             ELSSLCCP
00105                                                                   ELSSLCCP
00106  01  WS-SCREEN-LINE.                                              ELSSLCCP
00107      05  WS-STATUS-MSG        PIC X(14).                          ELSSLCCP
00108          88  WS-NO-GROUP-SPEC           VALUE 'NO GROUP SPEC.'.   ELSSLCCP
00109          88  WS-APPLICABLE              VALUE SPACES.             ELSSLCCP
00110          88  WS-NOT-APPLICABLE          VALUE '0'.                ELSSLCCP
00111      05  FILLER               PIC X.                              ELSSLCCP
00112      05  WS-CHOICE            PIC Z9.                             ELSSLCCP
00113      05  FILLER               PIC XXX.                            ELSSLCCP
00114      05  WS-TEXT              PIC X(48).                          ELSSLCCP
00115      05  WS-KEYWORD           PIC X(10).                          ELSSLCCP
00116      EJECT                                                        ELSSLCCP
00117  01  CCP-PROGRAM-TABLE-AREA.                                      ELSSLCCP
00118      05  CCP-NUM-HEADINGS    PIC S9(4) COMP    VALUE +5.          ELSSLCCP
00119      05  CCP-NUM-PROGRAMS    PIC S9(4) COMP    VALUE +23.         ELSSLCCP
00120      05  CCP-NUM-DETAILS     PIC S9(4) COMP    VALUE +1.          ELSSLCCP
00121                                                                   ELSSLCCP
00122      05  CCP-ERROR-HEADING-1    PIC X(79)      VALUE              ELSSLCCP
00123          'THE GROUP SPECIFIC RECORD WAS NOT FOUND FOR THIS GROUP SELSSLCCP
00124 -        'ECTION.'.                                               ELSSLCCP
00125      05  CCP-ERROR-HEADING-2    PIC X(79)      VALUE              ELSSLCCP
00126         '                  **** PLEASE RESELECT TO PROCEED ****'. ELSSLCCP
00127                                                                   ELSSLCCP
00128      05  CCP-HEADING-AREA.                                        ELSSLCCP
00129          10  CCP-HEADING-1      PIC X(79)      VALUE              ELSSLCCP
00130      'SELECT THE COST CONTAINMENT PROGRAM YOU WISH TO DISPLAY FROMELSSLCCP
00131 -    ' THE LIST BELOW '.                                          ELSSLCCP
00132          10  CCP-HEADING-2      PIC X(79)      VALUE              ELSSLCCP
00133      'AND PRESS THE <ENTER> KEY'.                                 ELSSLCCP
00134          10  CCP-HEADING-3      PIC X(79)      VALUE SPACES.      ELSSLCCP
00135          10  CCP-HEADING-4      PIC X(79)      VALUE              ELSSLCCP
00136      '            SELECT  COST CONTAINMENT PROGRAM                ELSSLCCP
00137 -    '        KEY WORD   '.                                       ELSSLCCP
00138          10  CCP-HEADING-5      PIC X(79)      VALUE SPACES.      ELSSLCCP
00139      05  CCP-HEADING            REDEFINES CCP-HEADING-AREA        ELSSLCCP
00140                                 OCCURS 5 TIMES                    ELSSLCCP
00141                                 PIC X(79).                        ELSSLCCP
00142                                                                   ELSSLCCP
00143      05  CCP-NO-PROGRAM-APPLIES-MSG.                              ELSSLCCP
00144          10  NO-CCP-APPLIES-1    PIC X(79)       VALUE            ELSSLCCP
00145              'NO COST CONTAINMENT PROGRAMS APPLY FOR THIS GROUP ANELSSLCCP
00146 -    'D SECTION NUMBER.  PLEASE'.                                 ELSSLCCP
00147          10  NO-CCP-APPLIES-2    PIC X(22)       VALUE            ELSSLCCP
00148              'PRESS <PF3> TO RETURN.'.                            ELSSLCCP
00149                                                                   ELSSLCCP
00150      05  CCP-PROGRAM-DEFINITION.                                  ELSSLCCP
00151 *                                                                 ELSSLCCP
00152          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00153              '01ADDITIONAL TRANSPLANT COVERAGE PROGRAM'.          ELSSLCCP
00154          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00155              'ATCP'.                                              ELSSLCCP
00156          10  CCP-ADDL-TRNSPLNT-COVRG-IND                          ELSSLCCP
00157                                 PIC XX   VALUE SPACE.             ELSSLCCP
00158 *                                                                 ELSSLCCP
00159          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00160              '03BLUE ADVANTAGE ENTREPRENEUR'.                     ELSSLCCP
00161          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00162              'BAE'.                                               ELSSLCCP
00163          10  CCP-COMM-BLUE-ADV-ENT-IND                            ELSSLCCP
00164                                 PIC XX   VALUE SPACE.             ELSSLCCP
00165 *                                                                 ELSSLCCP
00166 *                                                                 ELSSLCCP
00167          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00168              '03COMMUNITY BLUE OPTION PROGRAM'.                   ELSSLCCP
00169          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00170              'CBL'.                                               ELSSLCCP
00171          10  CCP-COMM-BLUE-PART-OPTION-IND                        ELSSLCCP
00172                                 PIC XX   VALUE SPACE.             ELSSLCCP
00173 *                                                                 ELSSLCCP
00174          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00175              '04COMMUNITY PARTICIPATING OPTION PROGRAM'.          ELSSLCCP
00176          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00177              'CPO'.                                               ELSSLCCP
00178          10  CCP-COMM-PARTICIPAT-OPTION-IND                       ELSSLCCP
00179                                 PIC XX   VALUE SPACE.             ELSSLCCP
00180 *                                                                 ELSSLCCP
00181          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00182              '05EXTENDED MENTAL HEALTH PROGRAM'.                  ELSSLCCP
00183          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00184              'EMH'.                                               ELSSLCCP
00185          10  CCP-EXT-MENTAL-HEALTH-IND                            ELSSLCCP
00186                                 PIC XX   VALUE SPACE.             ELSSLCCP
00187 *                                                                 ELSSLCCP
00188          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00189              '06HMO MANAGED CARE NETWORK PROGRAM'.                ELSSLCCP
00190          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00191              'HMC'.                                               ELSSLCCP
00192          10  CCP-HMO-MC-INDICATOR                                 ELSSLCCP
00193                                 PIC XX   VALUE SPACE.             ELSSLCCP
00194 *                                                                 ELSSLCCP
00195          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00196              '07HOSPICE PROGRAM'.                                 ELSSLCCP
00197          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00198              'HOSP'.                                              ELSSLCCP
00199          10  CCP-HOSPICE-IND    PIC XX   VALUE SPACE.             ELSSLCCP
00200 *                                                                 ELSSLCCP
00201          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00202              '08INCENTIVE OBSTETRICAL PROGRAM'.                   ELSSLCCP
00203          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00204              'IOB'.                                               ELSSLCCP
00205          10  CCP-INCENTIVE-OB-IND                                 ELSSLCCP
00206                                 PIC XX   VALUE SPACE.             ELSSLCCP
00207 *                                                                 ELSSLCCP
00208          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00209              '09MANAGED CARE NETWORK PROGRAM'.                    ELSSLCCP
00210          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00211              'MCN'.                                               ELSSLCCP
00212          10  CCP-POS-PARTICP-IND                                  ELSSLCCP
00213                                 PIC XX   VALUE SPACE.             ELSSLCCP
00214 *                                                                 ELSSLCCP
00215          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00216              '10MANDATORY ADDITIONAL SURGICAL OPINION PROGRAM'.   ELSSLCCP
00217          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00218              'MASOP'.                                             ELSSLCCP
00219          10  CCP-MAND-ADDL-SURG-OPN-IND                           ELSSLCCP
00220                                 PIC XX   VALUE SPACE.             ELSSLCCP
00221 *                                                                 ELSSLCCP
00222          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00223              '11MANDATORY OUTPATIENT SURGERY PROGRAM'.            ELSSLCCP
00224          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00225              'MOPS'.                                              ELSSLCCP
00226          10  CCP-MAND-OP-SURG-PROG-IND                            ELSSLCCP
00227                                 PIC XX   VALUE SPACE.             ELSSLCCP
00228 *                                                                 ELSSLCCP
00229          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00230             '12MEDICAL NECESSITY PROGRAM'.                        ELSSLCCP
00231          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00232             'MEDNEC'.                                             ELSSLCCP
00233          10  CCP-MED-NECESSITY-HCNR-IPS-IN                        ELSSLCCP
00234                                 PIC XX   VALUE SPACE.             ELSSLCCP
00235 *                                                                 ELSSLCCP
00236          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00237              '13MEDICAL SERVICES ADVISORY PROGRAM'.               ELSSLCCP
00238          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00239              'MSA'.                                               ELSSLCCP
00240          10  CCP-MED-SERV-ADV-PROG-IND                            ELSSLCCP
00241                                 PIC XX   VALUE SPACE.             ELSSLCCP
00242 *                                                                 ELSSLCCP
00243          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00244              '14MENTAL HEALTH SUBSTANCE ABUSE PROGRAM'.           ELSSLCCP
00245          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00246              'MHSC'.                                              ELSSLCCP
00247          10  CCP-MENTAL-SUBS-ABUSE-IND                            ELSSLCCP
00248                                 PIC XX   VALUE SPACE.             ELSSLCCP
00249 *                                                                 ELSSLCCP
00250          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00251              '15MONDAY DISCHARGE PROGRAM'.                        ELSSLCCP
00252          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00253              'MOND'.                                              ELSSLCCP
00254          10  CCP-MONDAY-DISCHARGE-IND                             ELSSLCCP
00255                                 PIC XX   VALUE SPACE.             ELSSLCCP
00256 *                                                                 ELSSLCCP
00257          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00258              '16POINT OF SERVICE PROGRAM'.                        ELSSLCCP
00259          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00260              'POS'.                                               ELSSLCCP
00261          10  CCP-POINT-OF-SERVICE-IND                             ELSSLCCP
00262                                 PIC XX   VALUE SPACE.             ELSSLCCP
00263 *                                                                 ELSSLCCP
00264          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00265              '17PREFERRED ANCILLARY NETWORK PROGRAM'.             ELSSLCCP
00266          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00267              'PAN'.                                               ELSSLCCP
00268          10  CCP-PARTICIPAT-PAN-OPTION                            ELSSLCCP
00269                                 PIC XX   VALUE SPACE.             ELSSLCCP
00270 *                                                                 ELSSLCCP
00271          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00272              '18PREFERRED PROVIDER OPTION PROGRAM'.               ELSSLCCP
00273          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00274              'PPO'.                                               ELSSLCCP
00275          10  CCP-PARTICIPAT-PROV-OPTION                           ELSSLCCP
00276                                 PIC XX   VALUE SPACE.             ELSSLCCP
00277 *                                                                 ELSSLCCP
00278          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00279              '19PRE-ADMISSION REVIEW PROGRAM'.                    ELSSLCCP
00280          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00281              'PAR'.                                               ELSSLCCP
00282          10  CCP-PRE-ADM-REVIEW-IND                               ELSSLCCP
00283                                 PIC XX   VALUE SPACE.             ELSSLCCP
00284 *                                                                 ELSSLCCP
00285          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00286              '20PRE-ADMISSION TESTING PROGRAM'.                   ELSSLCCP
00287          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00288              'PAT'.                                               ELSSLCCP
00289          10  CCP-PRE-ADM-TESTING-PROGRAM                          ELSSLCCP
00290                                 PIC XX   VALUE SPACE.             ELSSLCCP
00291 *                                                                 ELSSLCCP
00292          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00293             '21REIMBURSEMENT/SUBROGATION PROGRAM'.                ELSSLCCP
00294          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00295              'REIMB'.                                             ELSSLCCP
00296          10  CCP-REIMBUR-SUBROG-IND                               ELSSLCCP
00297                                 PIC XX   VALUE SPACE.             ELSSLCCP
00298 *                                                                 ELSSLCCP
00299          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00300             '22RESTRICTED PROVIDER OPTION PROGRAM'.               ELSSLCCP
00301          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00302              'RPO'.                                               ELSSLCCP
00303          10  CCP-RESTRICT-PROV-IND                                ELSSLCCP
00304                                 PIC XX   VALUE SPACE.             ELSSLCCP
00305 *                                                                 ELSSLCCP
00306          10  FILLER             PIC X(50)            VALUE        ELSSLCCP
00307              '23WEEKEND ADMISSION PROGRAM'.                       ELSSLCCP
00308          10  FILLER             PIC X(10)            VALUE        ELSSLCCP
00309              'WEEK'.                                              ELSSLCCP
00310          10  CCP-FRI-SAT-ADM-IND                                  ELSSLCCP
00311                                 PIC XX   VALUE SPACE.             ELSSLCCP
00312                                                                   ELSSLCCP
00313      05  CCP-PROGRAM-TABLE  REDEFINES CCP-PROGRAM-DEFINITION      ELSSLCCP
00314                             OCCURS 23 TIMES                       ELSSLCCP
00315                             INDEXED BY CCP-IDX.                   ELSSLCCP
00316          10  CCP-CHOICE     PIC XX.                               ELSSLCCP
00317          10  CCP-TEXT       PIC X(48).                            ELSSLCCP
00318          10  CCP-KEYWORD    PIC X(10).                            ELSSLCCP
00319          10  CCP-AVAIL-IND  PIC XX.                               ELSSLCCP
00320              88  CCP-NO-GROUP-SPEC  VALUE SPACE.                  ELSSLCCP
00321              88  CCP-NOT-APPLICABLE VALUE '00'.                   ELSSLCCP
00322      EJECT                                                        ELSSLCCP
00323  LINKAGE SECTION.                                                 ELSSLCCP
00324                                                                   ELSSLCCP
00325  01  DFHCOMMAREA.                                                 ELSSLCCP
00326      COPY ELSCOMMC.                                               ELSSLCCP
00327      EJECT                                                        ELSSLCCP
00328      COPY ELSCIA2C.                                               ELSSLCCP
00329      EJECT                                                        ELSSLCCP
00330      COPY ELSIOPMC.                                               ELSSLCCP
00331      EJECT                                                        ELSSLCCP
00332      COPY ELSSSCBC.                                               ELSSLCCP
00333      EJECT                                                        ELSSLCCP
00334      COPY ELSMENUC.                                               ELSSLCCP
00335      EJECT                                                        ELSSLCCP
00336      COPY ELSMHDGC.                                               ELSSLCCP
00337      EJECT                                                        ELSSLCCP
00338      COPY ELSMOPTC.                                               ELSSLCCP
00339      EJECT                                                        ELSSLCCP
00340      COPY ELSKEYSC.                                               ELSSLCCP
00341      EJECT                                                        ELSSLCCP
00342      COPY ELSKTBGC.                                               ELSSLCCP
00343      EJECT                                                        ELSSLCCP
00344  01  GROUP-SPECIFIC-RECORD.                                       ELSSLCCP
00345      COPY GCGROUPC.                                               ELSSLCCP
00346      EJECT                                                        ELSSLCCP
00347  PROCEDURE DIVISION.                                              ELSSLCCP
00348 ************************************************************      ELSSLCCP
00349 *                                                          *      ELSSLCCP
00350 *                    PROCEDURE DIVISION                    *      ELSSLCCP
00351 *                                                          *      ELSSLCCP
00352 ************************************************************      ELSSLCCP
00353                                                                   ELSSLCCP
00354                                                                   ELSSLCCP
00355 ************************************************************      ELSSLCCP
00356 *                                                          *      ELSSLCCP
00357 *        PERFORM COST CONTAINMENT FUNCTIONS                *      ELSSLCCP
00358 *                                                          *      ELSSLCCP
00359 ************************************************************      ELSSLCCP
00360  PERFORM-COST-CONTAINMENT-FUNCT.                                  ELSSLCCP
00361      PERFORM INITIALIZE-MODULE.                                   ELSSLCCP
00362      PERFORM PROCESS-COST-CONTAINMENT.                            ELSSLCCP
00363      GOBACK.                                                      ELSSLCCP
00364                                                                   ELSSLCCP
00365                                                                   ELSSLCCP
00366 ************************************************************      ELSSLCCP
00367 *                                                          *      ELSSLCCP
00368 *        INITIALIZE MODULE                                 *      ELSSLCCP
00369 *                                                          *      ELSSLCCP
00370 ************************************************************      ELSSLCCP
00371  INITIALIZE-MODULE.                                               ELSSLCCP
00372      PERFORM CHECK-COMMAREA-LENGTH.                               ELSSLCCP
00373      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSSLCCP
00374      PERFORM ESTABLISH-ADDRESSING-TO-SELECT.                      ELSSLCCP
00375      EJECT                                                        ELSSLCCP
00376                                                                   ELSSLCCP
00377                                                                   ELSSLCCP
00378 ************************************************************      ELSSLCCP
00379 *                                                          *      ELSSLCCP
00380 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSSLCCP
00381 *                                                          *      ELSSLCCP
00382 ************************************************************      ELSSLCCP
00383  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSSLCCP
00384      IF ECA-CIA-PTR IS NOT EQUAL NULL                             ELSSLCCP
00385          PERFORM SET-CIA-ADDRESS                                  ELSSLCCP
00386      ELSE                                                         ELSSLCCP
00387          PERFORM SIGNAL-CIA-ADDRESSING-ERROR.                     ELSSLCCP
00388                                                                   ELSSLCCP
00389                                                                   ELSSLCCP
00390 ************************************************************      ELSSLCCP
00391 *                                                          *      ELSSLCCP
00392 *        SET CIA ADDRESS                                   *      ELSSLCCP
00393 *                                                          *      ELSSLCCP
00394 ************************************************************      ELSSLCCP
00395  SET-CIA-ADDRESS.                                                 ELSSLCCP
00396      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSSLCCP
00397          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELSSLCCP
00398                                                                   ELSSLCCP
00399                                                                   ELSSLCCP
00400 ************************************************************      ELSSLCCP
00401 *                                                          *      ELSSLCCP
00402 *        SIGNAL CIA ADDRESSING ERROR                       *      ELSSLCCP
00403 *                                                          *      ELSSLCCP
00404 ************************************************************      ELSSLCCP
00405  SIGNAL-CIA-ADDRESSING-ERROR.                                     ELSSLCCP
00406      EXEC CICS ABEND                                              ELSSLCCP
00407                ABCODE('EL02')                                     ELSSLCCP
00408                END-EXEC.                                          ELSSLCCP
00409      EJECT                                                        ELSSLCCP
00410                                                                   ELSSLCCP
00411                                                                   ELSSLCCP
00412 ************************************************************      ELSSLCCP
00413 *                                                          *      ELSSLCCP
00414 *        ESTABLISH ADDRESSING TO SELECTOR CONTROL AREA     *      ELSSLCCP
00415 *                                                          *      ELSSLCCP
00416 ************************************************************      ELSSLCCP
00417  ESTABLISH-ADDRESSING-TO-SELECT.                                  ELSSLCCP
00418      PERFORM SET-SSCB-ADDRESS.                                    ELSSLCCP
00419      IF CIA-RC-PTR-NULL                                           ELSSLCCP
00420          PERFORM SIGNAL-MISSING-PARAMETER.                        ELSSLCCP
00421                                                                   ELSSLCCP
00422                                                                   ELSSLCCP
00423 ************************************************************      ELSSLCCP
00424 *                                                          *      ELSSLCCP
00425 *        SET SSCB ADDRESS                                  *      ELSSLCCP
00426 *                                                          *      ELSSLCCP
00427 ************************************************************      ELSSLCCP
00428  SET-SSCB-ADDRESS.                                                ELSSLCCP
00429      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSSLCCP
00430      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLCCP
00431          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSSLCCP
00432      EJECT                                                        ELSSLCCP
00433                                                                   ELSSLCCP
00434                                                                   ELSSLCCP
00435 ************************************************************      ELSSLCCP
00436 *                                                          *      ELSSLCCP
00437 *        PROCESS COST CONTAINMENT                          *      ELSSLCCP
00438 *                                                          *      ELSSLCCP
00439 ************************************************************      ELSSLCCP
00440  PROCESS-COST-CONTAINMENT.                                        ELSSLCCP
00441      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSSLCCP
00442          PERFORM PROCESS-BUILD-COST-CONTAINMENT                   ELSSLCCP
00443      ELSE IF SSB-MENU-COMPLETE(SSB-SELECTOR-STATE)                ELSSLCCP
00444          PERFORM PROCESS-MENU-COMPLETED-REQUEST                   ELSSLCCP
00445      ELSE                                                         ELSSLCCP
00446          PERFORM SIGNAL-INVALID-COST-CONT-SELEC.                  ELSSLCCP
00447                                                                   ELSSLCCP
00448                                                                   ELSSLCCP
00449 ************************************************************      ELSSLCCP
00450 *                                                          *      ELSSLCCP
00451 *        PROCESS BUILD COST CONTAINMENT MENU REQUEST       *      ELSSLCCP
00452 *                                                          *      ELSSLCCP
00453 ************************************************************      ELSSLCCP
00454  PROCESS-BUILD-COST-CONTAINMENT.                                  ELSSLCCP
00455      MOVE WS-COST-MNU-TITLE  TO SSB-MNU-TITLE.                    ELSSLCCP
00456      MOVE WS-COST-PROGRAM-TITLE TO                                ELSSLCCP
00457          SSB-MODIFIER-1-PHRASE.                                   ELSSLCCP
00458      PERFORM READ-GROUP-SPECIFIC-RECORD.                          ELSSLCCP
00459      PERFORM PROCESS-BUILD-MENU-REQUEST.                          ELSSLCCP
00460      EJECT                                                        ELSSLCCP
00461                                                                   ELSSLCCP
00462                                                                   ELSSLCCP
00463 ************************************************************      ELSSLCCP
00464 *                                                          *      ELSSLCCP
00465 *        READ GROUP SPECIFIC RECORD                        *      ELSSLCCP
00466 *                                                          *      ELSSLCCP
00467 ************************************************************      ELSSLCCP
00468  READ-GROUP-SPECIFIC-RECORD.                                      ELSSLCCP
00469      PERFORM INITIALIZE-READ.                                     ELSSLCCP
00470      PERFORM LOOK-UP-THE-GROUP.                                   ELSSLCCP
00471      IF WS-GROUP-FOUND                                            ELSSLCCP
00472          PERFORM READ-THE-GROUP-RECORD.                           ELSSLCCP
00473                                                                   ELSSLCCP
00474                                                                   ELSSLCCP
00475 ************************************************************      ELSSLCCP
00476 *                                                          *      ELSSLCCP
00477 *        INITIALIZE READ                                   *      ELSSLCCP
00478 *                                                          *      ELSSLCCP
00479 ************************************************************      ELSSLCCP
00480  INITIALIZE-READ.                                                 ELSSLCCP
00481      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSSLCCP
00482      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLCCP
00483          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELSSLCCP
00484      IF CIA-RC-PTR-NULL                                           ELSSLCCP
00485          PERFORM ALLOCATE-GROUP-SPECIFIC-AREA.                    ELSSLCCP
00486                                                                   ELSSLCCP
00487      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELSSLCCP
00488      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLCCP
00489          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELSSLCCP
00490      IF CIA-RC-PTR-NULL                                           ELSSLCCP
00491         PERFORM ALLOCATE-KEY-AREA                                 ELSSLCCP
00492         SET CIA-ELSKEYS-DDN TO TRUE                               ELSSLCCP
00493         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSSLCCP
00494             ADDRESS OF KWA-FILE-KEY-WORK-AREA.                    ELSSLCCP
00495                                                                   ELSSLCCP
00496      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELSSLCCP
00497      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLCCP
00498          ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                       ELSSLCCP
00499      IF CIA-RC-PTR-NULL                                           ELSSLCCP
00500         PERFORM ALLOCATE-KTG-TABLE                                ELSSLCCP
00501         SET CIA-ELSKTBG-DDN TO TRUE                               ELSSLCCP
00502         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSSLCCP
00503             ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                    ELSSLCCP
00504      EJECT                                                        ELSSLCCP
00505                                                                   ELSSLCCP
00506                                                                   ELSSLCCP
00507 ************************************************************      ELSSLCCP
00508 *                                                          *      ELSSLCCP
00509 *        ALLOCATE GROUP SPECIFIC AREA                      *      ELSSLCCP
00510 *                                                          *      ELSSLCCP
00511 ************************************************************      ELSSLCCP
00512  ALLOCATE-GROUP-SPECIFIC-AREA.                                    ELSSLCCP
00513      SET CIA-GCGRPSPC-DDN TO TRUE.                                ELSSLCCP
00514      SET CIA-STG-GETMAIN  TO TRUE.                                ELSSLCCP
00515      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSLCCP
00516      EJECT                                                        ELSSLCCP
00517                                                                   ELSSLCCP
00518                                                                   ELSSLCCP
00519 ************************************************************      ELSSLCCP
00520 *                                                          *      ELSSLCCP
00521 *        ALLOCATE KEY AREA                                 *      ELSSLCCP
00522 *                                                          *      ELSSLCCP
00523 ************************************************************      ELSSLCCP
00524  ALLOCATE-KEY-AREA.                                               ELSSLCCP
00525      SET CIA-ELSKEYS-DDN  TO TRUE.                                ELSSLCCP
00526      SET CIA-STG-GETMAIN  TO TRUE.                                ELSSLCCP
00527      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSLCCP
00528      EJECT                                                        ELSSLCCP
00529                                                                   ELSSLCCP
00530                                                                   ELSSLCCP
00531 ************************************************************      ELSSLCCP
00532 *                                                          *      ELSSLCCP
00533 *        ALLOCATE KTG TABLE                                *      ELSSLCCP
00534 *                                                          *      ELSSLCCP
00535 ************************************************************      ELSSLCCP
00536  ALLOCATE-KTG-TABLE.                                              ELSSLCCP
00537      SET CIA-ELSKTBG-DDN  TO TRUE.                                ELSSLCCP
00538      SET CIA-STG-RETRIEVE TO TRUE.                                ELSSLCCP
00539      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSLCCP
00540      EJECT                                                        ELSSLCCP
00541                                                                   ELSSLCCP
00542                                                                   ELSSLCCP
00543 ************************************************************      ELSSLCCP
00544 *                                                          *      ELSSLCCP
00545 *        LOOK UP THE GROUP                                 *      ELSSLCCP
00546 *                                                          *      ELSSLCCP
00547 ************************************************************      ELSSLCCP
00548  LOOK-UP-THE-GROUP.                                               ELSSLCCP
00549      SET KTG-IDX TO 1.                                            ELSSLCCP
00550      SEARCH KTG-KEY-TBL VARYING KTG-IDX                           ELSSLCCP
00551          AT END                                                   ELSSLCCP
00552              SET WS-GROUP-NOT-FOUND TO TRUE                       ELSSLCCP
00553          WHEN KTG-SEL (KTG-IDX)                                   ELSSLCCP
00554              SET WS-GROUP-FOUND TO TRUE                           ELSSLCCP
00555        END-SEARCH.                                                ELSSLCCP
00556      EJECT                                                        ELSSLCCP
00557                                                                   ELSSLCCP
00558                                                                   ELSSLCCP
00559 ************************************************************      ELSSLCCP
00560 *                                                          *      ELSSLCCP
00561 *        READ THE GROUP RECORD                             *      ELSSLCCP
00562 *                                                          *      ELSSLCCP
00563 ************************************************************      ELSSLCCP
00564  READ-THE-GROUP-RECORD.                                           ELSSLCCP
00565      SET CIA-GCGRPSPC-DDN   TO TRUE.                              ELSSLCCP
00566      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLCCP
00567          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSSLCCP
00568      PERFORM BUILD-GROUP-SPECIFIC-KEY.                            ELSSLCCP
00569      SET CIA-GCGRPSPC-DDN   TO TRUE.                              ELSSLCCP
00570      SET IOP-REC-PTR        TO  NULL.                             ELSSLCCP
00571      SET IOP-RD             TO  TRUE.                             ELSSLCCP
00572      SET IOP-FCQ-NONE       TO  TRUE.                             ELSSLCCP
00573      SET IOP-KVQ-EQ         TO  TRUE.                             ELSSLCCP
00574      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSLCCP
00575      IF IOP-RC-OK                                                 ELSSLCCP
00576          PERFORM GET-COST-CONTAINMENT-OPTIONS                     ELSSLCCP
00577      ELSE                                                         ELSSLCCP
00578          PERFORM SIGNAL-GROUP-NOT-FOUND.                          ELSSLCCP
00579      EJECT                                                        ELSSLCCP
00580                                                                   ELSSLCCP
00581                                                                   ELSSLCCP
00582 ************************************************************      ELSSLCCP
00583 *                                                          *      ELSSLCCP
00584 *        BUILD GROUP SPECIFIC KEY                          *      ELSSLCCP
00585 *                                                          *      ELSSLCCP
00586 ************************************************************      ELSSLCCP
00587  BUILD-GROUP-SPECIFIC-KEY.                                        ELSSLCCP
00588      MOVE SSB-PLAN-CODE              TO                           ELSSLCCP
00589          KWA-GCG-PLAN-CODE.                                       ELSSLCCP
00590      MOVE SSB-GROUP-NUMBER            TO                          ELSSLCCP
00591          KWA-GCG-GROUP-NUMBER                                     ELSSLCCP
00592      MOVE SSB-SECTN-NO                TO  KWA-GCG-SECTION-NUMBER. ELSSLCCP
00593      MOVE SSB-PKG-CODE                TO  KWA-GCG-PKG-CODE.       ELSSLCCP
00594      MOVE KTG-FAM-REL-LVL (KTG-IDX)   TO  KWA-GCG-FAM-REL-LVL.    ELSSLCCP
00595      MOVE KTG-EFF-DT-CENTURY (KTG-IDX) TO                         ELSSLCCP
00596                                     KWA-GCG-EFF-DATE-CENTURY.     ELSSLCCP
00597      MOVE KWA-GCGRPSPC-KEY            TO  IOP-FILE-KEY.           ELSSLCCP
00598      EJECT                                                        ELSSLCCP
00599                                                                   ELSSLCCP
00600                                                                   ELSSLCCP
00601 ************************************************************      ELSSLCCP
00602 *                                                          *      ELSSLCCP
00603 *        GET COST CONTAINMENT OPTIONS                      *      ELSSLCCP
00604 *                                                          *      ELSSLCCP
00605 ************************************************************      ELSSLCCP
00606  GET-COST-CONTAINMENT-OPTIONS.                                    ELSSLCCP
00607      SET ADDRESS OF GROUP-SPECIFIC-RECORD                         ELSSLCCP
00608           TO IOP-REC-PTR.                                         ELSSLCCP
00609      MOVE GCG-ADDL-TRNSPLNT-COVRG-IND                             ELSSLCCP
00610          TO CCP-ADDL-TRNSPLNT-COVRG-IND.                          ELSSLCCP
00611      MOVE GCG-FRI-SAT-ADM-IND                                     ELSSLCCP
00612          TO CCP-FRI-SAT-ADM-IND.                                  ELSSLCCP
00613      MOVE GCG-HOSPICE-IND                                         ELSSLCCP
00614          TO CCP-HOSPICE-IND.                                      ELSSLCCP
00615      MOVE GCG-INCENTIVE-OB-IND                                    ELSSLCCP
00616          TO CCP-INCENTIVE-OB-IND.                                 ELSSLCCP
00617      MOVE GCG-MAND-ADDL-SURG-OPN-IND                              ELSSLCCP
00618          TO CCP-MAND-ADDL-SURG-OPN-IND.                           ELSSLCCP
00619      MOVE GCG-MED-NECESSITY-HCNR-IPS-IN                           ELSSLCCP
00620          TO CCP-MED-NECESSITY-HCNR-IPS-IN.                        ELSSLCCP
00621      MOVE GCG-MAND-OP-SURG-PROG-IND                               ELSSLCCP
00622          TO CCP-MAND-OP-SURG-PROG-IND.                            ELSSLCCP
00623      MOVE GCG-MED-SERV-ADV-PROG-IND                               ELSSLCCP
00624          TO CCP-MED-SERV-ADV-PROG-IND.                            ELSSLCCP
00625      MOVE GCG-POS-PARTICP-IND                                     ELSSLCCP
00626          TO CCP-POS-PARTICP-IND.                                  ELSSLCCP
00627      MOVE GCG-HMO-MC-INDICATOR                                    ELSSLCCP
00628          TO CCP-HMO-MC-INDICATOR.                                 ELSSLCCP
00629      MOVE GCG-PRE-ADM-REVIEW-IND                                  ELSSLCCP
00630          TO CCP-PRE-ADM-REVIEW-IND.                               ELSSLCCP
00631      MOVE GCG-REIMBUR-SUBROG-IND                                  ELSSLCCP
00632          TO CCP-REIMBUR-SUBROG-IND.                               ELSSLCCP
00633      MOVE GCG-MONDAY-DISCHARGE-IND                                ELSSLCCP
00634          TO CCP-MONDAY-DISCHARGE-IND.                             ELSSLCCP
00635      MOVE GCG-PARTICIPAT-PROV-OPTION                              ELSSLCCP
00636          TO CCP-PARTICIPAT-PROV-OPTION.                           ELSSLCCP
00637      MOVE GCG-PRE-ADM-TESTING-PROGRAM                             ELSSLCCP
00638          TO CCP-PRE-ADM-TESTING-PROGRAM.                          ELSSLCCP
00639      MOVE GCG-SUBS-ABUSE-MENTAL-IND                               ELSSLCCP
00640          TO CCP-EXT-MENTAL-HEALTH-IND.                            ELSSLCCP
00641      MOVE GCG-NEW-MEN-SUB-ABUSE-IND                               ELSSLCCP
00642          TO CCP-MENTAL-SUBS-ABUSE-IND.                            ELSSLCCP
00643      MOVE GCG-NEW-POS-IND                                         ELSSLCCP
00644          TO CCP-POINT-OF-SERVICE-IND.                             ELSSLCCP
00645      MOVE GCG-RPO-INDICATOR                                       ELSSLCCP
00646          TO CCP-RESTRICT-PROV-IND.                                ELSSLCCP
00647      MOVE GCG-BAE-INDICATOR                                       ELSSLCCP
00648          TO CCP-COMM-BLUE-ADV-ENT-IND.                            ELSSLCCP
00649      MOVE GCG-CPO-PARTICIPATION-IND                               ELSSLCCP
00650          TO CCP-COMM-PARTICIPAT-OPTION-IND.                       ELSSLCCP
00651      MOVE GCG-CBL-PARTICIPATION-IND                               ELSSLCCP
00652          TO CCP-COMM-BLUE-PART-OPTION-IND.                        ELSSLCCP
00653      MOVE GCG-PAN-PARTICIPATION-IND                               ELSSLCCP
00654          TO CCP-PARTICIPAT-PAN-OPTION.                            ELSSLCCP
00655      EJECT                                                        ELSSLCCP
00656                                                                   ELSSLCCP
00657                                                                   ELSSLCCP
00658 ************************************************************      ELSSLCCP
00659 *                                                          *      ELSSLCCP
00660 *        PROCESS BUILD MENU REQUEST                        *      ELSSLCCP
00661 *                                                          *      ELSSLCCP
00662 ************************************************************      ELSSLCCP
00663  PROCESS-BUILD-MENU-REQUEST.                                      ELSSLCCP
00664      INITIALIZE SSB-MNU-CHOICE (1).                               ELSSLCCP
00665      PERFORM DELETE-MENU-FILE.                                    ELSSLCCP
00666      PERFORM ACQUIRE-STORAGE-AREAS.                               ELSSLCCP
00667      PERFORM BUILD-MENU-HEADERS.                                  ELSSLCCP
00668      PERFORM BUILD-MENU-BODY.                                     ELSSLCCP
00669      SET SSB-START-MENU (SSB-SELECTOR-STATE) TO TRUE.             ELSSLCCP
00670                                                                   ELSSLCCP
00671                                                                   ELSSLCCP
00672 ************************************************************      ELSSLCCP
00673 *                                                          *      ELSSLCCP
00674 *        DELETE MENU FILE                                  *      ELSSLCCP
00675 *                                                          *      ELSSLCCP
00676 ************************************************************      ELSSLCCP
00677  DELETE-MENU-FILE.                                                ELSSLCCP
00678      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSSLCCP
00679      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLCCP
00680          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELSSLCCP
00681      IF CIA-RC-PTR-NULL                                           ELSSLCCP
00682         PERFORM ALLOCATE-MENU-AREA                                ELSSLCCP
00683         SET CIA-ELSMENU-DDN  TO TRUE                              ELSSLCCP
00684         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSSLCCP
00685             ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.               ELSSLCCP
00686      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSSLCCP
00687      SET IOP-DEL          TO TRUE.                                ELSSLCCP
00688      SET IOP-FCQ-NONE     TO TRUE.                                ELSSLCCP
00689      SET IOP-KVQ-NONE     TO TRUE.                                ELSSLCCP
00690      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSLCCP
00691      EJECT                                                        ELSSLCCP
00692                                                                   ELSSLCCP
00693                                                                   ELSSLCCP
00694 ************************************************************      ELSSLCCP
00695 *                                                          *      ELSSLCCP
00696 *        ALLOCATE MENU AREA                                *      ELSSLCCP
00697 *                                                          *      ELSSLCCP
00698 ************************************************************      ELSSLCCP
00699  ALLOCATE-MENU-AREA.                                              ELSSLCCP
00700      SET CIA-ELSMENU-DDN  TO TRUE.                                ELSSLCCP
00701      SET CIA-STG-GETMAIN  TO TRUE.                                ELSSLCCP
00702      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSLCCP
00703      EJECT                                                        ELSSLCCP
00704                                                                   ELSSLCCP
00705                                                                   ELSSLCCP
00706 ************************************************************      ELSSLCCP
00707 *                                                          *      ELSSLCCP
00708 *        ACQUIRE STORAGE AREAS                             *      ELSSLCCP
00709 *                                                          *      ELSSLCCP
00710 ************************************************************      ELSSLCCP
00711  ACQUIRE-STORAGE-AREAS.                                           ELSSLCCP
00712      PERFORM GET-HEADING-STORAGE-AREA.                            ELSSLCCP
00713      PERFORM GET-SELECTION-CODE-KEYWORD-ARE.                      ELSSLCCP
00714      PERFORM GET-DESCRIPTION-LINE-AREA.                           ELSSLCCP
00715                                                                   ELSSLCCP
00716                                                                   ELSSLCCP
00717 ************************************************************      ELSSLCCP
00718 *                                                          *      ELSSLCCP
00719 *        GET HEADING STORAGE AREA                          *      ELSSLCCP
00720 *                                                          *      ELSSLCCP
00721 ************************************************************      ELSSLCCP
00722  GET-HEADING-STORAGE-AREA.                                        ELSSLCCP
00723      SET  CIA-ELSMHDG-DDN TO TRUE.                                ELSSLCCP
00724      COMPUTE CIA-AREA-LEN = LENGTH OF MHD-NBR-HDG-LINES +         ELSSLCCP
00725               (LENGTH OF MHD-HDG-LINE *                           ELSSLCCP
00726          CCP-NUM-HEADINGS).                                       ELSSLCCP
00727      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSLCCP
00728      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSLCCP
00729      SET CIA-ELSMHDG-DDN TO TRUE.                                 ELSSLCCP
00730      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLCCP
00731          ADDRESS OF MHD-MENU-HEADINGS.                            ELSSLCCP
00732                                                                   ELSSLCCP
00733                                                                   ELSSLCCP
00734 ************************************************************      ELSSLCCP
00735 *                                                          *      ELSSLCCP
00736 *        GET SELECTION CODE KEYWORD AREA                   *      ELSSLCCP
00737 *                                                          *      ELSSLCCP
00738 ************************************************************      ELSSLCCP
00739  GET-SELECTION-CODE-KEYWORD-ARE.                                  ELSSLCCP
00740      SET  CIA-ELSMOPT-DDN TO TRUE.                                ELSSLCCP
00741      COMPUTE CIA-AREA-LEN = LENGTH OF MSO-MENU-OPTS-HEADER +      ELSSLCCP
00742              (LENGTH OF MSO-MENU-OPT *                            ELSSLCCP
00743          CCP-NUM-PROGRAMS).                                       ELSSLCCP
00744      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSLCCP
00745      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSLCCP
00746      SET CIA-ELSMOPT-DDN TO TRUE.                                 ELSSLCCP
00747      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSSLCCP
00748          ADDRESS OF MSO-MENU-SELECTION-VALUES.                    ELSSLCCP
00749      EJECT                                                        ELSSLCCP
00750                                                                   ELSSLCCP
00751                                                                   ELSSLCCP
00752 ************************************************************      ELSSLCCP
00753 *                                                          *      ELSSLCCP
00754 *        GET DESCRIPTION LINE AREA                         *      ELSSLCCP
00755 *                                                          *      ELSSLCCP
00756 ************************************************************      ELSSLCCP
00757  GET-DESCRIPTION-LINE-AREA.                                       ELSSLCCP
00758      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSLCCP
00759      SET CIA-STG-GETMAIN TO TRUE.                                 ELSSLCCP
00760      SET IOP-GETMAIN-REC TO TRUE.                                 ELSSLCCP
00761      COMPUTE IOP-REC-LEN = LENGTH OF MSD-NBR-DESCR-LINES +        ELSSLCCP
00762                (LENGTH OF MSD-DESCR-LINE *                        ELSSLCCP
00763          CCP-NUM-DETAILS).                                        ELSSLCCP
00764      PERFORM CALL-STORAGE-SUBPROGRAM.                             ELSSLCCP
00765      SET ADDRESS OF MSD-MENU-ITEM-DESCRIPTIONS TO IOP-REC-PTR.    ELSSLCCP
00766                                                                   ELSSLCCP
00767                                                                   ELSSLCCP
00768 ************************************************************      ELSSLCCP
00769 *                                                          *      ELSSLCCP
00770 *        CALL STORAGE SUBPROGRAM                           *      ELSSLCCP
00771 *                                                          *      ELSSLCCP
00772 ************************************************************      ELSSLCCP
00773  CALL-STORAGE-SUBPROGRAM.                                         ELSSLCCP
00774      EXEC CICS LINK PROGRAM ('ELUSTGMG')                          ELSSLCCP
00775                     COMMAREA (DFHCOMMAREA)                        ELSSLCCP
00776                     END-EXEC.                                     ELSSLCCP
00777      EJECT                                                        ELSSLCCP
00778                                                                   ELSSLCCP
00779                                                                   ELSSLCCP
00780 ************************************************************      ELSSLCCP
00781 *                                                          *      ELSSLCCP
00782 *        BUILD MENU HEADERS                                *      ELSSLCCP
00783 *                                                          *      ELSSLCCP
00784 ************************************************************      ELSSLCCP
00785  BUILD-MENU-HEADERS.                                              ELSSLCCP
00786      MOVE CCP-NUM-HEADINGS TO MHD-NBR-HDG-LINES.                  ELSSLCCP
00787      SET MHD-IDX TO 1.                                            ELSSLCCP
00788      PERFORM LOAD-MENU-HEADINGS                                   ELSSLCCP
00789          VARYING MOVE-SUB FROM 1 BY 1 UNTIL                       ELSSLCCP
00790                     MOVE-SUB > CCP-NUM-HEADINGS.                  ELSSLCCP
00791      IF WS-GROUP-NOT-FOUND                                        ELSSLCCP
00792          PERFORM LOAD-NO-GROUP-HEADINGS.                          ELSSLCCP
00793                                                                   ELSSLCCP
00794                                                                   ELSSLCCP
00795 ************************************************************      ELSSLCCP
00796 *                                                          *      ELSSLCCP
00797 *        LOAD MENU HEADINGS                                *      ELSSLCCP
00798 *                                                          *      ELSSLCCP
00799 ************************************************************      ELSSLCCP
00800  LOAD-MENU-HEADINGS.                                              ELSSLCCP
00801      MOVE CCP-HEADING (MOVE-SUB) TO MHD-HDG-LINE                  ELSSLCCP
00802          (MHD-IDX).                                               ELSSLCCP
00803      SET MHD-IDX UP BY 1.                                         ELSSLCCP
00804                                                                   ELSSLCCP
00805                                                                   ELSSLCCP
00806 ************************************************************      ELSSLCCP
00807 *                                                          *      ELSSLCCP
00808 *        LOAD NO GROUP HEADINGS                            *      ELSSLCCP
00809 *                                                          *      ELSSLCCP
00810 ************************************************************      ELSSLCCP
00811  LOAD-NO-GROUP-HEADINGS.                                          ELSSLCCP
00812      MOVE CCP-ERROR-HEADING-1  TO  MHD-HDG-LINE (1).              ELSSLCCP
00813      MOVE CCP-ERROR-HEADING-2  TO  MHD-HDG-LINE (2).              ELSSLCCP
00814      EJECT                                                        ELSSLCCP
00815                                                                   ELSSLCCP
00816                                                                   ELSSLCCP
00817 ************************************************************      ELSSLCCP
00818 *                                                          *      ELSSLCCP
00819 *        BUILD MENU BODY                                   *      ELSSLCCP
00820 *                                                          *      ELSSLCCP
00821 ************************************************************      ELSSLCCP
00822  BUILD-MENU-BODY.                                                 ELSSLCCP
00823      PERFORM INITIALIZE-BUILD-MENU-BODY.                          ELSSLCCP
00824      PERFORM LOAD-TOPIC-NAMES                                     ELSSLCCP
00825          VARYING MOVE-SUB FROM 1 BY 1                             ELSSLCCP
00826                     UNTIL MOVE-SUB IS GREATER THAN                ELSSLCCP
00827              CCP-NUM-PROGRAMS.                                    ELSSLCCP
00828      MOVE WS-APPLICABLE-NUM           TO MSO-NBR-MENU-OPTS.       ELSSLCCP
00829      IF WS-APPLICABLE-NUM = ZERO                                  ELSSLCCP
00830         PERFORM VARYING MHD-IDX FROM 1 BY 1                       ELSSLCCP
00831             UNTIL MHD-IDX > CCP-NUM-HEADINGS                      ELSSLCCP
00832             MOVE SPACES TO MHD-HDG-LINE (MHD-IDX)                 ELSSLCCP
00833         END-PERFORM                                               ELSSLCCP
00834         PERFORM MOVE-NO-CCP-APPLIES-MSG                           ELSSLCCP
00835      END-IF.                                                      ELSSLCCP
00836                                                                   ELSSLCCP
00837 ************************************************************      ELSSLCCP
00838 *                                                          *      ELSSLCCP
00839 *        MOVE NO CCP APPLIES MESSAGE                       *      ELSSLCCP
00840 *                                                          *      ELSSLCCP
00841 ************************************************************      ELSSLCCP
00842  MOVE-NO-CCP-APPLIES-MSG.                                         ELSSLCCP
00843      SET MHD-IDX TO 1.                                            ELSSLCCP
00844      MOVE NO-CCP-APPLIES-1 TO MHD-HDG-LINE (MHD-IDX).             ELSSLCCP
00845      SET MHD-IDX UP BY 1.                                         ELSSLCCP
00846      MOVE NO-CCP-APPLIES-2 TO MHD-HDG-LINE (MHD-IDX).             ELSSLCCP
00847                                                                   ELSSLCCP
00848 ************************************************************      ELSSLCCP
00849 *                                                          *      ELSSLCCP
00850 *        INITIALIZE BUILD MENU BODY                        *      ELSSLCCP
00851 *                                                          *      ELSSLCCP
00852 ************************************************************      ELSSLCCP
00853  INITIALIZE-BUILD-MENU-BODY.                                      ELSSLCCP
00854      SET MSD-IDX TO 1.                                            ELSSLCCP
00855      MOVE CCP-NUM-DETAILS             TO                          ELSSLCCP
00856          MSD-NBR-DESCR-LINES.                                     ELSSLCCP
00857      MOVE CCP-NUM-PROGRAMS            TO MSO-NBR-MENU-OPTS.       ELSSLCCP
00858      MOVE 1   TO MSO-MIN-CHOICES                                  ELSSLCCP
00859                  MSO-MAX-CHOICES.                                 ELSSLCCP
00860      MOVE LENGTH OF WS-CHOICE TO MSO-OPT-LEN.                     ELSSLCCP
00861      SET MSO-OPT-TYP-NUM  TO TRUE.                                ELSSLCCP
00862      SET MSO-IDX TO 1.                                            ELSSLCCP
00863      SET IOP-ADD       TO TRUE.                                   ELSSLCCP
00864      SET IOP-FCQ-NONE  TO TRUE.                                   ELSSLCCP
00865      SET IOP-KVQ-NONE  TO TRUE.                                   ELSSLCCP
00866      SET CIA-ELSMENU-DDN TO TRUE.                                 ELSSLCCP
00867      EJECT                                                        ELSSLCCP
00868                                                                   ELSSLCCP
00869                                                                   ELSSLCCP
00870 ************************************************************      ELSSLCCP
00871 *                                                          *      ELSSLCCP
00872 *        LOAD TOPIC NAMES                                  *      ELSSLCCP
00873 *                                                          *      ELSSLCCP
00874 ************************************************************      ELSSLCCP
00875  LOAD-TOPIC-NAMES.                                                ELSSLCCP
00876      PERFORM FORMAT-SCREEN-LINE.                                  ELSSLCCP
00877      PERFORM DETERMINE-IF-APPLICABLE.                             ELSSLCCP
00878      IF WS-APPLICABLE                                             ELSSLCCP
00879          PERFORM ADD-TO-VALID-OPTIONS                             ELSSLCCP
00880          PERFORM WRITE-MENU-LINE.                                 ELSSLCCP
00881                                                                   ELSSLCCP
00882                                                                   ELSSLCCP
00883 ************************************************************      ELSSLCCP
00884 *                                                          *      ELSSLCCP
00885 *        FORMAT SCREEN LINE                                *      ELSSLCCP
00886 *                                                          *      ELSSLCCP
00887 ************************************************************      ELSSLCCP
00888  FORMAT-SCREEN-LINE.                                              ELSSLCCP
00889      SET CCP-IDX TO MOVE-SUB.                                     ELSSLCCP
00890      MOVE SPACES               TO WS-SCREEN-LINE.                 ELSSLCCP
00891      MOVE CCP-TEXT    (CCP-IDX) TO WS-TEXT.                       ELSSLCCP
00892      MOVE CCP-KEYWORD (CCP-IDX) TO WS-KEYWORD.                    ELSSLCCP
00893      EJECT                                                        ELSSLCCP
00894                                                                   ELSSLCCP
00895 ************************************************************      ELSSLCCP
00896 *                                                          *      ELSSLCCP
00897 *        DETERMINE IF APPLICABLE                           *      ELSSLCCP
00898 *                                                          *      ELSSLCCP
00899 ************************************************************      ELSSLCCP
00900  DETERMINE-IF-APPLICABLE.                                         ELSSLCCP
00901      IF CCP-NO-GROUP-SPEC (MOVE-SUB)                              ELSSLCCP
00902          PERFORM FLAG-NO-GROUP-SPEC                               ELSSLCCP
00903      ELSE IF CCP-NOT-APPLICABLE (MOVE-SUB)                        ELSSLCCP
00904          PERFORM FLAG-NOT-APPLICABLE-PROGRAM.                     ELSSLCCP
00905                                                                   ELSSLCCP
00906                                                                   ELSSLCCP
00907 ************************************************************      ELSSLCCP
00908 *                                                          *      ELSSLCCP
00909 *        FLAG NO GROUP SPEC                                *      ELSSLCCP
00910 *                                                          *      ELSSLCCP
00911 ************************************************************      ELSSLCCP
00912  FLAG-NO-GROUP-SPEC.                                              ELSSLCCP
00913      SET WS-NO-GROUP-SPEC   TO TRUE.                              ELSSLCCP
00914                                                                   ELSSLCCP
00915                                                                   ELSSLCCP
00916 ************************************************************      ELSSLCCP
00917 *                                                          *      ELSSLCCP
00918 *        FLAG NOT APPLICABLE PROGRAM                       *      ELSSLCCP
00919 *                                                          *      ELSSLCCP
00920 ************************************************************      ELSSLCCP
00921  FLAG-NOT-APPLICABLE-PROGRAM.                                     ELSSLCCP
00922      SET WS-NOT-APPLICABLE TO TRUE.                               ELSSLCCP
00923                                                                   ELSSLCCP
00924                                                                   ELSSLCCP
00925                                                                   ELSSLCCP
00926 ************************************************************      ELSSLCCP
00927 *                                                          *      ELSSLCCP
00928 *        ADD TO VALID OPTIONS                              *      ELSSLCCP
00929 *                                                          *      ELSSLCCP
00930 ************************************************************      ELSSLCCP
00931  ADD-TO-VALID-OPTIONS.                                            ELSSLCCP
00932      MOVE SPACES TO MSO-OPT-SEL (MSO-IDX).                        ELSSLCCP
00933      SET CCP-IDX TO MOVE-SUB.                                     ELSSLCCP
00934      ADD 1 TO WS-APPLICABLE-CTR.                                  ELSSLCCP
00935      MOVE WS-APPLICABLE-CTR                                       ELSSLCCP
00936         TO WS-APPLICABLE-NUM.                                     ELSSLCCP
00937      MOVE WS-APPLICABLE-NUM TO                                    ELSSLCCP
00938           MSO-OPT-NUM-2 (MSO-IDX)                                 ELSSLCCP
00939           WS-CHOICE.                                              ELSSLCCP
00940      MOVE CCP-KEYWORD (CCP-IDX) TO                                ELSSLCCP
00941           MSO-OPT-KWD (MSO-IDX).                                  ELSSLCCP
00942      SET MSO-IDX UP BY 1.                                         ELSSLCCP
00943                                                                   ELSSLCCP
00944 ************************************************************      ELSSLCCP
00945 *                                                          *      ELSSLCCP
00946 *        WRITE MENU LINE                                   *      ELSSLCCP
00947 *                                                          *      ELSSLCCP
00948 ************************************************************      ELSSLCCP
00949  WRITE-MENU-LINE.                                                 ELSSLCCP
00950      MOVE WS-SCREEN-LINE TO MSD-DESCR-LINE (MSD-IDX).             ELSSLCCP
00951      MOVE LENGTH OF MSD-MENU-ITEM-DESCRIPTIONS TO                 ELSSLCCP
00952          IOP-REC-LEN.                                             ELSSLCCP
00953      PERFORM CALL-INPUT-OUTPUT-SUBPROGRAM.                        ELSSLCCP
00954                                                                   ELSSLCCP
00955                                                                   ELSSLCCP
00956 ************************************************************      ELSSLCCP
00957 *                                                          *      ELSSLCCP
00958 *        PROCESS MENU COMPLETED REQUEST                    *      ELSSLCCP
00959 *                                                          *      ELSSLCCP
00960 ************************************************************      ELSSLCCP
00961  PROCESS-MENU-COMPLETED-REQUEST.                                  ELSSLCCP
00962      PERFORM GET-THE-MENU-TITLE.                                  ELSSLCCP
00963      MOVE SSB-MNU-CHOICE (1) TO SSB-MODIFIER-1.                   ELSSLCCP
00964      SET SSB-COMPLETED (SSB-SELECTOR-STATE)     TO TRUE.          ELSSLCCP
00965      EJECT                                                        ELSSLCCP
00966                                                                   ELSSLCCP
00967                                                                   ELSSLCCP
00968 ************************************************************      ELSSLCCP
00969 *                                                          *      ELSSLCCP
00970 *        GET THE MENU TITLE                                *      ELSSLCCP
00971 *                                                          *      ELSSLCCP
00972 ************************************************************      ELSSLCCP
00973  GET-THE-MENU-TITLE.                                              ELSSLCCP
00974      SET CCP-IDX TO 1.                                            ELSSLCCP
00975      SEARCH CCP-PROGRAM-TABLE VARYING CCP-IDX                     ELSSLCCP
00976          AT END                                                   ELSSLCCP
00977              SET CIA-AB-PARM-ERR TO TRUE                          ELSSLCCP
00978              EXEC CICS ABEND                                      ELSSLCCP
00979                        ABCODE(CIA-ABCODE)                         ELSSLCCP
00980              END-EXEC                                             ELSSLCCP
00981          WHEN                                                     ELSSLCCP
00982              CCP-KEYWORD (CCP-IDX) = SSB-MNU-CHOICE (1)           ELSSLCCP
00983                 MOVE CCP-TEXT (CCP-IDX) TO                        ELSSLCCP
00984          SSB-MODIFIER-1-PHRASE                                    ELSSLCCP
00985        END-SEARCH.                                                ELSSLCCP
00986                                                                   ELSSLCCP
00987                                                                   ELSSLCCP
00988 ************************************************************      ELSSLCCP
00989 *                                                          *      ELSSLCCP
00990 *        CHECK COMMAREA LENGTH                             *      ELSSLCCP
00991 *                                                          *      ELSSLCCP
00992 ************************************************************      ELSSLCCP
00993  CHECK-COMMAREA-LENGTH.                                           ELSSLCCP
00994      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSSLCCP
00995          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSSLCCP
00996                                                                   ELSSLCCP
00997                                                                   ELSSLCCP
00998 ************************************************************      ELSSLCCP
00999 *                                                          *      ELSSLCCP
01000 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSSLCCP
01001 *                                                          *      ELSSLCCP
01002 ************************************************************      ELSSLCCP
01003  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSSLCCP
01004      EXEC CICS ABEND                                              ELSSLCCP
01005                ABCODE('EL01')                                     ELSSLCCP
01006                END-EXEC.                                          ELSSLCCP
01007      EJECT                                                        ELSSLCCP
01008                                                                   ELSSLCCP
01009                                                                   ELSSLCCP
01010 ************************************************************      ELSSLCCP
01011 *                                                          *      ELSSLCCP
01012 *        CALL INPUT-OUTPUT SUBPROGRAM                      *      ELSSLCCP
01013 *                                                          *      ELSSLCCP
01014 ************************************************************      ELSSLCCP
01015  CALL-INPUT-OUTPUT-SUBPROGRAM.                                    ELSSLCCP
01016      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELSSLCCP
01017                     COMMAREA(DFHCOMMAREA)                         ELSSLCCP
01018                     END-EXEC.                                     ELSSLCCP
01019                                                                   ELSSLCCP
01020                                                                   ELSSLCCP
01021 ************************************************************      ELSSLCCP
01022 *                                                          *      ELSSLCCP
01023 *        SIGNAL MISSING PARAMETER                          *      ELSSLCCP
01024 *                                                          *      ELSSLCCP
01025 ************************************************************      ELSSLCCP
01026  SIGNAL-MISSING-PARAMETER.                                        ELSSLCCP
01027      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSSLCCP
01028      EXEC CICS ABEND ABCODE(CIA-ABCODE)                           ELSSLCCP
01029                END-EXEC.                                          ELSSLCCP
01030      EJECT                                                        ELSSLCCP
01031                                                                   ELSSLCCP
01032                                                                   ELSSLCCP
01033 ************************************************************      ELSSLCCP
01034 *                                                          *      ELSSLCCP
01035 *        SIGNAL INVALID COST CONT SELECTOR REQUEST         *      ELSSLCCP
01036 *                                                          *      ELSSLCCP
01037 ************************************************************      ELSSLCCP
01038  SIGNAL-INVALID-COST-CONT-SELEC.                                  ELSSLCCP
01039      SET CIA-AB-UNDEF TO TRUE.                                    ELSSLCCP
01040      EXEC CICS ABEND                                              ELSSLCCP
01041                ABCODE(CIA-ABCODE)                                 ELSSLCCP
01042                END-EXEC.                                          ELSSLCCP
01043      EJECT                                                        ELSSLCCP
01044                                                                   ELSSLCCP
01045                                                                   ELSSLCCP
01046 ************************************************************      ELSSLCCP
01047 *                                                          *      ELSSLCCP
01048 *        SIGNAL GROUP NOT FOUND                            *      ELSSLCCP
01049 *                                                          *      ELSSLCCP
01050 ************************************************************      ELSSLCCP
01051  SIGNAL-GROUP-NOT-FOUND.                                          ELSSLCCP
01052      SET CIA-AB-NOTFND-GCGRPSPC TO TRUE.                          ELSSLCCP
01053      EXEC CICS ABEND                                              ELSSLCCP
01054                ABCODE(CIA-ABCODE)                                 ELSSLCCP
01055                END-EXEC.                                          ELSSLCCP
