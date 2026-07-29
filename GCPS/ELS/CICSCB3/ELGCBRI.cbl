00001 *      LAST MAINTENANCE TIME: 11.21.35  DATE: 10/19/90            09/03/03
00002 *      LAST MAINTENANCE TIME: 11.21.35  DATE: 10/19/90            ELGCBRI 
00003  IDENTIFICATION DIVISION.                                            LV002
00004                                                                   ELGCBRI 
00005  PROGRAM-ID.         ELGCBRI.                                     ELGCBRI 
00006                                                                   ELGCBRI 
00007  AUTHOR.             RICK BARILEAU.                               ELGCBRI 
00008                                                                   ELGCBRI 
00009  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGCBRI 
00010                      A MUTUAL LEGAL RESERVE COMPANY               ELGCBRI 
00011                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGCBRI 
00012                      233 N. MICHIGAN AVE                          ELGCBRI 
00013                      CHICAGO, ILLINOIS 60601                      ELGCBRI 
00014                                                                   ELGCBRI 
00015  DATE-WRITTEN.       12-AUG-1987.                                 ELGCBRI 
00016                                                                   ELGCBRI 
00017  DATE-COMPILED.                                                   ELGCBRI 
00018                                                                   ELGCBRI 
00019  SECURITY.           COPYRIGHT 1986,                              ELGCBRI 
00020                      HEALTH CARE SERVICE CORPORATION              ELGCBRI 
00021      SKIP3                                                        ELGCBRI 
00022  ENVIRONMENT DIVISION.                                            ELGCBRI 
00023                                                                   ELGCBRI 
00024  CONFIGURATION SECTION.                                           ELGCBRI 
00025  SOURCE-COMPUTER.    IBM-3090.                                    ELGCBRI 
00026  OBJECT-COMPUTER.    IBM-3090.                                    ELGCBRI 
00027      EJECT                                                        ELGCBRI 
00028 ******************************************************************ELGCBRI 
00029 *                                                                *ELGCBRI 
00030 *  ELGCBRI  - ELS:  GENERATES THE OUTPUT FOR COMBINED BENEFITS   *ELGCBRI 
00031 *                   REDUCTION INDICATOR AT THE COST CONTAINMENT  *ELGCBRI 
00032 *                   LEVEL.                                       *ELGCBRI 
00033 *                                                                *ELGCBRI 
00034 ******************************************************************ELGCBRI 
00035 *                                                                *ELGCBRI 
00036 *                      MAINTENANCE HISTORY                       *ELGCBRI 
00037 *                                                                *ELGCBRI 
00038 *  MOD     DATE     BY  DRPT                ACTION               *ELGCBRI 
00039 * ----- ----------- --- ----- ---------------------------------- *ELGCBRI 
00040 * 01.00 12-AUG-1987 REB       CREATED                            *ELGCBRI 
00041 *                                                                *ELGCBRI 
00042 * 01.01 22-AUG-1990 AKK       ADDED SUBSTANCE ABUSE/MENTAL AND   *ELGCBRI 
00043 *                             MANAGED CARE NETWORK INFO TO PROG  *ELGCBRI 
00044 *                                                                *ELGCBRI 
00045 * 01.02 10-OCT-1990 JPB       CHANGED STORAGE MANAGEMENT         *ELGCBRI 
00046 *                                                                *ELGCBRI 
00047 * 01.03 16-FEB-1995 AKK       ADDED RPO, POS AND MHSC.           *ELGCBRI 
00048 *                                                                *ELGCBRI 
00049 * 01.04 20-FEB-1995 AKK       ADDED CPO                          *ELGCBRI 
00050 *                                                                *ELGCBRI 
00051 * 01.05 08-MAR-1996 AKK       ADDED CBL AND PAN.                 *ELGCBRI 
00052 *                                                                *ELGCBRI 
00053 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGCBRI 
00054 *                                                                *ELGCBRI 
00055 ******************************************************************ELGCBRI 
00056                                                                   ELGCBRI 
00057  DATA DIVISION.                                                   ELGCBRI 
00058  WORKING-STORAGE SECTION.                                         ELGCBRI 
00059 /                                                                 ELGCBRI 
00060                                                                   ELGCBRI 
00061  01  COST-CONT-SUB-HEADERS.                                       ELGCBRI 
00062      05  CBL-HEADER                  PIC  X(16) VALUE             ELGCBRI 
00063            ' COMMUNITY BLUE '.                                    ELGCBRI 
00064      05  CPO-HEADER                  PIC  X(34) VALUE             ELGCBRI 
00065            ' COMMUNITY PARTICIPATING OPTION '.                    ELGCBRI 
00066      05  WEEKN-HEADER                PIC  X(19) VALUE             ELGCBRI 
00067            ' WEEKEND ADMISSION '.                                 ELGCBRI 
00068      05  HOSP-HEADER                 PIC  X(09) VALUE             ELGCBRI 
00069            ' HOSPICE '.                                           ELGCBRI 
00070      05  MCN-HEADER                  PIC  X(22) VALUE             ELGCBRI 
00071            ' MANAGED CARE NETWORK '.                              ELGCBRI 
00072      05  MASOP-HEADER                PIC  X(39) VALUE             ELGCBRI 
00073            ' MANDATORY ADDITIONAL SURGICAL OPINION '.             ELGCBRI 
00074      05  MONDAY-HEADER               PIC  X(18) VALUE             ELGCBRI 
00075            ' MONDAY DISCHARGE'.                                   ELGCBRI 
00076      05  MEDNEC-HEADER               PIC  X(19) VALUE             ELGCBRI 
00077            ' MEDICAL NECESSITY '.                                 ELGCBRI 
00078      05  MOPS-HEADER                 PIC  X(30) VALUE             ELGCBRI 
00079            ' MANDATORY OUTPATIENT SURGERY '.                      ELGCBRI 
00080      05  MSA-HEADER                  PIC  X(27) VALUE             ELGCBRI 
00081            ' MEDICAL SERVICES ADVISORY '.                         ELGCBRI 
00082      05  POS-HEADER                  PIC  X(25) VALUE             ELGCBRI 
00083            ' POINT OF SERVICE OPTION '.                           ELGCBRI 
00084      05  PAN-HEADER                  PIC  X(37) VALUE             ELGCBRI 
00085            ' PREFERRED ANCILLARY PROVIDER OPTION '.               ELGCBRI 
00086      05  PPO-HEADER                  PIC  X(27) VALUE             ELGCBRI 
00087            ' PREFERRED PROVIDER OPTION '.                         ELGCBRI 
00088      05  PAR-HEADER                  PIC  X(22) VALUE             ELGCBRI 
00089            ' PRE-ADMISSION REVIEW '.                              ELGCBRI 
00090      05  RPO-HEADER                  PIC  X(28) VALUE             ELGCBRI 
00091            ' RESTRICTED PROVIDER OPTION '.                        ELGCBRI 
00092      05  SAM-HEADER                  PIC  X(24) VALUE             ELGCBRI 
00093            ' SUBSTANCE ABUSE/MENTAL '.                            ELGCBRI 
00094                                                                   ELGCBRI 
00095  01  MESSAGES.                                                    ELGCBRI 
00096      05  CCP-NAME-NO-MATCH-MSG       PIC  X(79) VALUE             ELGCBRI 
00097      'THERE IS NO MATCH FOR THE COMBINED BENEFITS REDUCTION INDICAELGCBRI 
00098 -    'TOR.'.                                                      ELGCBRI 
00099                                                                   ELGCBRI 
00100  01  PROGRAM-CONSTANTS.                                           ELGCBRI 
00101      05  PC-AND-OR                   PIC  X(08) VALUE ' AND/OR '. ELGCBRI 
00102      05  PC-COMBINED                 PIC  X(18) VALUE             ELGCBRI 
00103          ' IS COMBINED WITH '.                                    ELGCBRI 
00104      05  PC-COMMA                    PIC  X(01) VALUE ','.        ELGCBRI 
00105      05  PC-GCCP                     PIC  X(06) VALUE '#GCCP '.   ELGCBRI 
00106      05  PC-WHEN                     PIC  X(05) VALUE 'WHEN '.    ELGCBRI 
00107                                                                   ELGCBRI 
00108  01  SWITCH.                                                      ELGCBRI 
00109      05  WS-MATCH-FOUND-SWITCH       PIC  X(01) VALUE SPACE.      ELGCBRI 
00110          88  NO-MATCH                           VALUE 'N'.        ELGCBRI 
00111          88  MATCH-FOUND                        VALUE 'Y'.        ELGCBRI 
00112                                                                   ELGCBRI 
00113  01  MISC.                                                        ELGCBRI 
00114      05  WS-COST-CONT-TYPE           PIC  X(02) VALUE SPACE.      ELGCBRI 
00115          88  WEEKEND                            VALUE 'FS'.       ELGCBRI 
00116          88  HOSPICE                            VALUE 'HO'.       ELGCBRI 
00117          88  MCN                                VALUE 'PS'.       ELGCBRI 
00118          88  MASOP                              VALUE 'MA'.       ELGCBRI 
00119          88  MOPS                               VALUE 'MO'.       ELGCBRI 
00120          88  MONDAY                             VALUE 'MD'.       ELGCBRI 
00121          88  MEDNEC                             VALUE 'MN'.       ELGCBRI 
00122          88  MSA                                VALUE 'MS'.       ELGCBRI 
00123          88  PPO                                VALUE 'PP'.       ELGCBRI 
00124          88  CBLUE                              VALUE 'CB'.       ELGCBRI 
00125          88  CPO                                VALUE 'CP'.       ELGCBRI 
00126          88  RPO                                VALUE 'RP'.       ELGCBRI 
00127          88  PAR                                VALUE 'PR'.       ELGCBRI 
00128          88  PAN                                VALUE 'PA'.       ELGCBRI 
00129          88  POS                                VALUE 'P1'.       ELGCBRI 
00130          88  SAM                                VALUE 'SA'.       ELGCBRI 
00131                                                                   ELGCBRI 
00132  01  WS-HOLD-SUB              COMP   PIC S9(04) VALUE +0.         ELGCBRI 
00133  01  WS-HOLD-SUB2             COMP   PIC S9(04) VALUE +0.         ELGCBRI 
00134                                                                   ELGCBRI 
00135  01  WS-HOLD-LINES-AREA.                                          ELGCBRI 
00136      05  WS-HOLD                     PIC  X(1580).                ELGCBRI 
00137      05  WS-HOLD-LINE  REDEFINES WS-HOLD                          ELGCBRI 
00138                                      PIC  X(79)                   ELGCBRI 
00139                                      OCCURS 20 TIMES.             ELGCBRI 
00140                                                                   ELGCBRI 
00141                                                                   ELGCBRI 
00142  LINKAGE SECTION.                                                 ELGCBRI 
00143  01  DFHCOMMAREA.                                                 ELGCBRI 
00144      COPY ELSCOMMC.                                               ELGCBRI 
00145                                                                   ELGCBRI 
00146      COPY ELSCIA2C.                                               ELGCBRI 
00147                                                                   ELGCBRI 
00148      COPY ELSCMIFC.                                               ELGCBRI 
00149                                                                   ELGCBRI 
00150      COPY ELSCMDSC.                                               ELGCBRI 
00151                                                                   ELGCBRI 
00152      COPY ELSOUTPC.                                               ELGCBRI 
00153                                                                   ELGCBRI 
00154      COPY ELSSRTPC.                                               ELGCBRI 
00155                                                                   ELGCBRI 
00156      COPY ELSSSCBC.                                               ELGCBRI 
00157                                                                   ELGCBRI 
00158      COPY ELSTCWAC.                                               ELGCBRI 
00159                                                                   ELGCBRI 
00160  01  GCCP-TABULAR-REC-AREA.                                       ELGCBRI 
00161      COPY GCTGCCPC.                                               ELGCBRI 
00162      EJECT                                                        ELGCBRI 
00163  PROCEDURE DIVISION.                                              ELGCBRI 
00164 ************************************************************      ELGCBRI 
00165 *                                                          *      ELGCBRI 
00166 *                    PROCEDURE DIVISION                    *      ELGCBRI 
00167 *                                                          *      ELGCBRI 
00168 ************************************************************      ELGCBRI 
00169                                                                   ELGCBRI 
00170                                                                   ELGCBRI 
00171 ************************************************************      ELGCBRI 
00172 *                                                          *      ELGCBRI 
00173 *        COMBINED BENEFITS REDUCTION IND                   *      ELGCBRI 
00174 *                                                          *      ELGCBRI 
00175 ************************************************************      ELGCBRI 
00176  COMBINED-BENEFITS-REDUCTION-IN.                                  ELGCBRI 
00177      PERFORM INITIALIZATION.                                      ELGCBRI 
00178      PERFORM PROCESS-CBRI.                                        ELGCBRI 
00179      GOBACK.                                                      ELGCBRI 
00180                                                                   ELGCBRI 
00181                                                                   ELGCBRI 
00182 ************************************************************      ELGCBRI 
00183 *                                                          *      ELGCBRI 
00184 *        INITIALIZATION                                    *      ELGCBRI 
00185 *                                                          *      ELGCBRI 
00186 ************************************************************      ELGCBRI 
00187  INITIALIZATION.                                                  ELGCBRI 
00188      PERFORM ESTABLISH-ADDRESSABILITY-OF-CN.                      ELGCBRI 
00189      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELGCBRI 
00190      INITIALIZE WS-HOLD-LINES-AREA.                               ELGCBRI 
00191                                                                   ELGCBRI 
00192                                                                   ELGCBRI 
00193 ************************************************************      ELGCBRI 
00194 *                                                          *      ELGCBRI 
00195 *        ESTABLISH ADDRESSABILITY OF CNTROL BLOCKS         *      ELGCBRI 
00196 *                                                          *      ELGCBRI 
00197 ************************************************************      ELGCBRI 
00198  ESTABLISH-ADDRESSABILITY-OF-CN.                                  ELGCBRI 
00199      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELGCBRI 
00200      PERFORM ESTABLISH-ADDRESSABILITY-OF-CM.                      ELGCBRI 
00201      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELGCBRI 
00202                                                                   ELGCBRI 
00203                                                                   ELGCBRI 
00204 ************************************************************      ELGCBRI 
00205 *                                                          *      ELGCBRI 
00206 *        CHECK FOR VALID COMMAREA                          *      ELGCBRI 
00207 *                                                          *      ELGCBRI 
00208 ************************************************************      ELGCBRI 
00209  CHECK-FOR-VALID-COMMAREA.                                        ELGCBRI 
00210      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCBRI 
00211          PERFORM SIGNAL-INVALID-COMMAREA.                         ELGCBRI 
00212                                                                   ELGCBRI 
00213                                                                   ELGCBRI 
00214 ************************************************************      ELGCBRI 
00215 *                                                          *      ELGCBRI 
00216 *        SIGNAL INVALID COMMAREA                           *      ELGCBRI 
00217 *                                                          *      ELGCBRI 
00218 ************************************************************      ELGCBRI 
00219  SIGNAL-INVALID-COMMAREA.                                         ELGCBRI 
00220      EXEC CICS ABEND                                              ELGCBRI 
00221                ABCODE('EL01')                                     ELGCBRI 
00222         END-EXEC.                                                 ELGCBRI 
00223      EJECT                                                        ELGCBRI 
00224                                                                   ELGCBRI 
00225                                                                   ELGCBRI 
00226 ************************************************************      ELGCBRI 
00227 *                                                          *      ELGCBRI 
00228 *        ESTABLISH ADDRESSABILITY OF CMMON INTERFACE AREA  *      ELGCBRI 
00229 *                                                          *      ELGCBRI 
00230 ************************************************************      ELGCBRI 
00231  ESTABLISH-ADDRESSABILITY-OF-CM.                                  ELGCBRI 
00232      IF ECA-CIA-PTR = NULL                                        ELGCBRI 
00233          PERFORM SIGNAL-INVALID-CIA                               ELGCBRI 
00234      ELSE                                                         ELGCBRI 
00235          PERFORM ESTABLISH-ADDRESS-OF-CIA.                        ELGCBRI 
00236                                                                   ELGCBRI 
00237                                                                   ELGCBRI 
00238 ************************************************************      ELGCBRI 
00239 *                                                          *      ELGCBRI 
00240 *        SIGNAL INVALID CIA                                *      ELGCBRI 
00241 *                                                          *      ELGCBRI 
00242 ************************************************************      ELGCBRI 
00243  SIGNAL-INVALID-CIA.                                              ELGCBRI 
00244      EXEC CICS ABEND                                              ELGCBRI 
00245                ABCODE('EL02')                                     ELGCBRI 
00246         END-EXEC.                                                 ELGCBRI 
00247                                                                   ELGCBRI 
00248                                                                   ELGCBRI 
00249 ************************************************************      ELGCBRI 
00250 *                                                          *      ELGCBRI 
00251 *        ESTABLISH ADDRESS OF CIA                          *      ELGCBRI 
00252 *                                                          *      ELGCBRI 
00253 ************************************************************      ELGCBRI 
00254  ESTABLISH-ADDRESS-OF-CIA.                                        ELGCBRI 
00255      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGCBRI 
00256                            ADDRESS OF                             ELGCBRI 
00257          CIA-ELS-COMMON-INTERFACE-AREA.                           ELGCBRI 
00258      EJECT                                                        ELGCBRI 
00259                                                                   ELGCBRI 
00260                                                                   ELGCBRI 
00261 ************************************************************      ELGCBRI 
00262 *                                                          *      ELGCBRI 
00263 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELGCBRI 
00264 *                                                          *      ELGCBRI 
00265 ************************************************************      ELGCBRI 
00266  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELGCBRI 
00267      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGCBRI 
00268      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCBRI 
00269                            ADDRESS OF                             ELGCBRI 
00270          SSB-SELECTOR-STATUS-CTL-BLK.                             ELGCBRI 
00271      IF CIA-RC-PTR-NULL                                           ELGCBRI 
00272          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCBRI 
00273                                                                   ELGCBRI 
00274                                                                   ELGCBRI 
00275 ************************************************************      ELGCBRI 
00276 *                                                          *      ELGCBRI 
00277 *        SIGNAL UNALLOC AREA ERROR                         *      ELGCBRI 
00278 *                                                          *      ELGCBRI 
00279 ************************************************************      ELGCBRI 
00280  SIGNAL-UNALLOC-AREA-ERROR.                                       ELGCBRI 
00281      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCBRI 
00282      PERFORM SIGNAL-ABEND.                                        ELGCBRI 
00283                                                                   ELGCBRI 
00284                                                                   ELGCBRI 
00285 ************************************************************      ELGCBRI 
00286 *                                                          *      ELGCBRI 
00287 *        SIGNAL ABEND                                      *      ELGCBRI 
00288 *                                                          *      ELGCBRI 
00289 ************************************************************      ELGCBRI 
00290  SIGNAL-ABEND.                                                    ELGCBRI 
00291      EXEC CICS ABEND                                              ELGCBRI 
00292                ABCODE(CIA-ABCODE)                                 ELGCBRI 
00293         END-EXEC.                                                 ELGCBRI 
00294      EJECT                                                        ELGCBRI 
00295                                                                   ELGCBRI 
00296                                                                   ELGCBRI 
00297 ************************************************************      ELGCBRI 
00298 *                                                          *      ELGCBRI 
00299 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELGCBRI 
00300 *                                                          *      ELGCBRI 
00301 ************************************************************      ELGCBRI 
00302  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELGCBRI 
00303      PERFORM ESTABLISH-ADDRESSABILITY-OF-CD.                      ELGCBRI 
00304      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELGCBRI 
00305      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELGCBRI 
00306      PERFORM ESTABLISH-ADDRESSABILITY-OF-SU.                      ELGCBRI 
00307      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELGCBRI 
00308                                                                   ELGCBRI 
00309                                                                   ELGCBRI 
00310 ************************************************************      ELGCBRI 
00311 *                                                          *      ELGCBRI 
00312 *        ESTABLISH ADDRESSABILITY OF CDES MANUAL INTERFACE *      ELGCBRI 
00313 *                                                          *      ELGCBRI 
00314 ************************************************************      ELGCBRI 
00315  ESTABLISH-ADDRESSABILITY-OF-CD.                                  ELGCBRI 
00316      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGCBRI 
00317      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCBRI 
00318                            ADDRESS OF                             ELGCBRI 
00319          CMF-CODES-MANUAL-INTERFACE.                              ELGCBRI 
00320      IF CIA-RC-PTR-NULL                                           ELGCBRI 
00321          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCBRI 
00322      EJECT                                                        ELGCBRI 
00323                                                                   ELGCBRI 
00324                                                                   ELGCBRI 
00325 ************************************************************      ELGCBRI 
00326 *                                                          *      ELGCBRI 
00327 *        ESTABLISH ADDRESSABILITY OF CST CONTAINMENT PROGRA*      ELGCBRI 
00328 *                                                          *      ELGCBRI 
00329 ************************************************************      ELGCBRI 
00330  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELGCBRI 
00331      SET CIA-ELSPGMW1-DDN TO TRUE.                                ELGCBRI 
00332      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCBRI 
00333                            ADDRESS OF                             ELGCBRI 
00334          GCCP-TABULAR-REC-AREA.                                   ELGCBRI 
00335      IF CIA-RC-PTR-NULL                                           ELGCBRI 
00336          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCBRI 
00337      EJECT                                                        ELGCBRI 
00338                                                                   ELGCBRI 
00339                                                                   ELGCBRI 
00340 ************************************************************      ELGCBRI 
00341 *                                                          *      ELGCBRI 
00342 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGCBRI 
00343 *                                                          *      ELGCBRI 
00344 ************************************************************      ELGCBRI 
00345  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELGCBRI 
00346      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGCBRI 
00347      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCBRI 
00348                            ADDRESS OF                             ELGCBRI 
00349          COF-OUTPUT-INTERFACE.                                    ELGCBRI 
00350      IF CIA-RC-PTR-NULL                                           ELGCBRI 
00351          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCBRI 
00352      EJECT                                                        ELGCBRI 
00353                                                                   ELGCBRI 
00354                                                                   ELGCBRI 
00355 ************************************************************      ELGCBRI 
00356 *                                                          *      ELGCBRI 
00357 *        ESTABLISH ADDRESSABILITY OF SUBROUTINE PARAMETERS *      ELGCBRI 
00358 *                                                          *      ELGCBRI 
00359 ************************************************************      ELGCBRI 
00360  ESTABLISH-ADDRESSABILITY-OF-SU.                                  ELGCBRI 
00361      SET CIA-ELSSRTP-DDN TO TRUE.                                 ELGCBRI 
00362      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCBRI 
00363                            ADDRESS OF                             ELGCBRI 
00364          SRP-SUBROUTINE-PARAMETERS.                               ELGCBRI 
00365      IF CIA-RC-PTR-NULL                                           ELGCBRI 
00366          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCBRI 
00367      EJECT                                                        ELGCBRI 
00368                                                                   ELGCBRI 
00369                                                                   ELGCBRI 
00370 ************************************************************      ELGCBRI 
00371 *                                                          *      ELGCBRI 
00372 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELGCBRI 
00373 *                                                          *      ELGCBRI 
00374 ************************************************************      ELGCBRI 
00375  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELGCBRI 
00376      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGCBRI 
00377      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCBRI 
00378                            ADDRESS OF                             ELGCBRI 
00379          TCAR-COMPRESSION-WORK-AREA.                              ELGCBRI 
00380      IF CIA-RC-PTR-NULL                                           ELGCBRI 
00381          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCBRI 
00382      EJECT                                                        ELGCBRI 
00383                                                                   ELGCBRI 
00384                                                                   ELGCBRI 
00385 ************************************************************      ELGCBRI 
00386 *                                                          *      ELGCBRI 
00387 *        PROCESS CBRI                                      *      ELGCBRI 
00388 *                                                          *      ELGCBRI 
00389 ************************************************************      ELGCBRI 
00390  PROCESS-CBRI.                                                    ELGCBRI 
00391      MOVE SRP-COST-CONT-TYPE TO WS-COST-CONT-TYPE.                ELGCBRI 
00392      SET GSS-INDEX TO +1.                                         ELGCBRI 
00393      PERFORM SEARCH-GCCP-RECORD-FOR-MATCHIN                       ELGCBRI 
00394          UNTIL GSS-INDEX NOT LESS THAN GSS-ENTRY-COUNT.           ELGCBRI 
00395      IF NO-MATCH                                                  ELGCBRI 
00396          PERFORM SEND-NOT-APPLY-MESSAGE                           ELGCBRI 
00397      ELSE                                                         ELGCBRI 
00398          PERFORM CREATE-COMBINED-BENEFIT-REDUCT.                  ELGCBRI 
00399      PERFORM SETUP-FOR-OUTPUT-LINK.                               ELGCBRI 
00400      EJECT                                                        ELGCBRI 
00401                                                                   ELGCBRI 
00402                                                                   ELGCBRI 
00403 ************************************************************      ELGCBRI 
00404 *                                                          *      ELGCBRI 
00405 *        SEARCH GCCP RECORD FOR MATCHING CBRI              *      ELGCBRI 
00406 *                                                          *      ELGCBRI 
00407 ************************************************************      ELGCBRI 
00408  SEARCH-GCCP-RECORD-FOR-MATCHIN.                                  ELGCBRI 
00409 **********************************************************        ELGCBRI 
00410 * THIS SEARCH IS LOOKING FOR A MATCHING VALUE IN THE     *        ELGCBRI 
00411 * COMBINED BENEFIT REDUCTION INDICATOR PASSED IN THE     *        ELGCBRI 
00412 * SRP BLOCK. WE ALSO WANT TO MAKE SURE THAT WE ELIMINATE *        ELGCBRI 
00413 * THE PROGRAM THAT IS CALLING THIS GENERATOR. IF A       *        ELGCBRI 
00414 * MATCH IS FOUND WE WANT THE NAME OF THAT CCP.           *        ELGCBRI 
00415 **********************************************************        ELGCBRI 
00416      SEARCH GSS-ENTRY                                             ELGCBRI 
00417             WHEN GSS-FS-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00418                  NOT WEEKEND AND                                  ELGCBRI 
00419                ((GSS-FS-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00420          EQUAL                                                    ELGCBRI 
00421                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00422                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00423                  MOVE WEEKN-HEADER TO                             ELGCBRI 
00424          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00425                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00426             WHEN GSS-HO-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00427                  NOT HOSPICE AND                                  ELGCBRI 
00428                ((GSS-HO-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00429          EQUAL                                                    ELGCBRI 
00430                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00431                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00432                  MOVE HOSP-HEADER TO                              ELGCBRI 
00433          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00434                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00435             WHEN GSS-PS-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00436                  NOT MCN AND                                      ELGCBRI 
00437                ((GSS-PS-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00438          EQUAL                                                    ELGCBRI 
00439                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00440                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00441                  MOVE MCN-HEADER TO                               ELGCBRI 
00442          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00443                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00444             WHEN GSS-MA-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00445                  NOT MASOP AND                                    ELGCBRI 
00446                ((GSS-MA-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00447          EQUAL                                                    ELGCBRI 
00448                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00449                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00450                  MOVE MASOP-HEADER TO                             ELGCBRI 
00451          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00452                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00453             WHEN GSS-MD-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00454                  NOT MONDAY AND                                   ELGCBRI 
00455                ((GSS-MD-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00456          EQUAL                                                    ELGCBRI 
00457                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00458                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00459                  MOVE MONDAY-HEADER TO                            ELGCBRI 
00460          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00461                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00462             WHEN GSS-MN-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00463                  NOT MEDNEC AND                                   ELGCBRI 
00464                ((GSS-MN-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00465          EQUAL                                                    ELGCBRI 
00466                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00467                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00468                  MOVE MEDNEC-HEADER TO                            ELGCBRI 
00469          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00470                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00471             WHEN GSS-MO-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00472                  NOT MOPS AND                                     ELGCBRI 
00473                ((GSS-MO-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00474          EQUAL                                                    ELGCBRI 
00475                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00476                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00477                  MOVE MOPS-HEADER TO                              ELGCBRI 
00478          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00479                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00480             WHEN GSS-MS-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00481                  NOT MSA AND                                      ELGCBRI 
00482                ((GSS-MS-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00483          EQUAL                                                    ELGCBRI 
00484                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00485                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00486                  MOVE MSA-HEADER TO                               ELGCBRI 
00487          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00488                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00489             WHEN GSS-PP-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00490                  NOT PPO AND                                      ELGCBRI 
00491                ((GSS-PP-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00492          EQUAL                                                    ELGCBRI 
00493                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00494                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00495                  MOVE PPO-HEADER TO                               ELGCBRI 
00496          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00497                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00498             WHEN GSS-PR-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00499                  NOT PAR AND                                      ELGCBRI 
00500                ((GSS-PR-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00501          EQUAL                                                    ELGCBRI 
00502                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00503                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00504                  MOVE PAR-HEADER TO                               ELGCBRI 
00505          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00506                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00507             WHEN GSS-SA-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00508                  NOT SAM AND                                      ELGCBRI 
00509                ((GSS-SA-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00510          EQUAL                                                    ELGCBRI 
00511                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00512                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00513                  MOVE SAM-HEADER TO                               ELGCBRI 
00514          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00515                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00516             WHEN GSS-RP-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00517                  NOT RPO AND                                      ELGCBRI 
00518                ((GSS-RP-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00519          EQUAL                                                    ELGCBRI 
00520                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00521                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00522                  MOVE RPO-HEADER TO                               ELGCBRI 
00523          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00524                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00525             WHEN GSS-CP-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00526                  NOT CPO AND                                      ELGCBRI 
00527                ((GSS-CP-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00528          EQUAL                                                    ELGCBRI 
00529                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00530                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00531                  MOVE CPO-HEADER TO                               ELGCBRI 
00532          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00533                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00534             WHEN GSS-CB-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00535                  NOT CBLUE AND                                    ELGCBRI 
00536                ((GSS-CB-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00537          EQUAL                                                    ELGCBRI 
00538                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00539                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00540                  MOVE CBL-HEADER TO                               ELGCBRI 
00541          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00542                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00543             WHEN GSS-P1-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00544                  NOT POS AND                                      ELGCBRI 
00545                ((GSS-P1-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00546          EQUAL                                                    ELGCBRI 
00547                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00548                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00549                  MOVE POS-HEADER TO                               ELGCBRI 
00550          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00551                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00552             WHEN GSS-PA-PROG-CODE-CHR (GSS-INDEX) AND             ELGCBRI 
00553                  NOT PAN AND                                      ELGCBRI 
00554                ((GSS-PA-COMB-BENE-REDUCT-IND (GSS-INDEX)          ELGCBRI 
00555          EQUAL                                                    ELGCBRI 
00556                  SRP-CCP-COMB-BENE-REDUCT-IND))                   ELGCBRI 
00557                  ADD +1 TO WS-HOLD-SUB                            ELGCBRI 
00558                  MOVE PAN-HEADER TO                               ELGCBRI 
00559          WS-HOLD-LINE(WS-HOLD-SUB)                                ELGCBRI 
00560                  SET MATCH-FOUND TO TRUE                          ELGCBRI 
00561         END-SEARCH.                                               ELGCBRI 
00562      SET GSS-INDEX UP BY +1.                                      ELGCBRI 
00563                                                                   ELGCBRI 
00564                                                                   ELGCBRI 
00565 ************************************************************      ELGCBRI 
00566 *                                                          *      ELGCBRI 
00567 *        SEND NOT APPLY MESSAGE                            *      ELGCBRI 
00568 *                                                          *      ELGCBRI 
00569 ************************************************************      ELGCBRI 
00570  SEND-NOT-APPLY-MESSAGE.                                          ELGCBRI 
00571      MOVE +1 TO COF-NBR-DTL-LINES.                                ELGCBRI 
00572      MOVE CCP-NAME-NO-MATCH-MSG TO                                ELGCBRI 
00573          COF-DTL-LINE(COF-NBR-DTL-LINES).                         ELGCBRI 
00574      EJECT                                                        ELGCBRI 
00575                                                                   ELGCBRI 
00576                                                                   ELGCBRI 
00577 ************************************************************      ELGCBRI 
00578 *                                                          *      ELGCBRI 
00579 *        TRANSLATE COMBINED BENEFIT REDUCTION INDICATOR    *      ELGCBRI 
00580 *                                                          *      ELGCBRI 
00581 ************************************************************      ELGCBRI 
00582  TRANSLATE-COMBINED-BENEFIT-RED.                                  ELGCBRI 
00583 ******************************************************************ELGCBRI 
00584 ** ALL THE COST CONTAINMENT PROGRAMS HAVE THE EXACT SAME CODE    *ELGCBRI 
00585 ** VALUES AND DEFINITIONS TO THEM. THE ONLY THING THAT VARIES    *ELGCBRI 
00586 ** IS THE SYSTEM NAME SO I JUST CHOSE ONE TO REPRESENT THEM ALL. *ELGCBRI 
00587 ******************************************************************ELGCBRI 
00588      MOVE SRP-CCP-COMB-BENE-REDUCT-IND    TO CMF-CODE-VALUE.      ELGCBRI 
00589      MOVE 'HO-COMB-BENE-REDUCT-IND'       TO                      ELGCBRI 
00590          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCBRI 
00591      PERFORM LINK-TO-CODES-MANUAL-FOR-TRANS.                      ELGCBRI 
00592      EJECT                                                        ELGCBRI 
00593                                                                   ELGCBRI 
00594                                                                   ELGCBRI 
00595 ************************************************************      ELGCBRI 
00596 *                                                          *      ELGCBRI 
00597 *        CREATE COMBINED BENEFIT REDUCTION SENTENCE        *      ELGCBRI 
00598 *                                                          *      ELGCBRI 
00599 ************************************************************      ELGCBRI 
00600  CREATE-COMBINED-BENEFIT-REDUCT.                                  ELGCBRI 
00601      PERFORM INITIALIZE-COMPRESS-AREA.                            ELGCBRI 
00602      ADD  +1   TO  TCAR-FROM-SUB.                                 ELGCBRI 
00603      STRING PC-WHEN ' ' SRP-CCP-NAME ' ' PC-COMBINED              ELGCBRI 
00604                 DELIMITED BY SIZE                                 ELGCBRI 
00605                 INTO TCAR-FROM-LINE (TCAR-FROM-SUB).              ELGCBRI 
00606      PERFORM MOVE-MATCHING-COST-CONTAINMENT                       ELGCBRI 
00607          VARYING WS-HOLD-SUB2 FROM +1 BY +1                       ELGCBRI 
00608               UNTIL WS-HOLD-SUB2 > WS-HOLD-SUB.                   ELGCBRI 
00609      PERFORM TRANSLATE-COMBINED-BENEFIT-RED.                      ELGCBRI 
00610      PERFORM MOVE-TRANSLATION-TO-COMPRESS-A                       ELGCBRI 
00611          VARYING CMF-DESCR-IDX FROM +1 BY +1                      ELGCBRI 
00612               UNTIL   CMF-DESCR-IDX > CMF-NBR-DESCR-LINES.        ELGCBRI 
00613      ADD  +1  TO  TCAR-FROM-SUB.                                  ELGCBRI 
00614      MOVE '.'  TO  TCAR-FROM-LINE (TCAR-FROM-SUB).                ELGCBRI 
00615      PERFORM CALL-TEXT-COMPRESSION-MODULE.                        ELGCBRI 
00616      PERFORM SETUP-FOR-UNSTRING-OPERATION.                        ELGCBRI 
00617      PERFORM CALL-TEXT-UNSTRING-MODULE.                           ELGCBRI 
00618      PERFORM MOVE-FORMATTED-TEXT-TO-OUTPUTX                       ELGCBRI 
00619          VARYING TCAR-X FROM +1 BY +1                             ELGCBRI 
00620               UNTIL   TCAR-X > TCAR-OUTPUT-FIELDS-USED.           ELGCBRI 
00621      EJECT                                                        ELGCBRI 
00622                                                                   ELGCBRI 
00623                                                                   ELGCBRI 
00624 ************************************************************      ELGCBRI 
00625 *                                                          *      ELGCBRI 
00626 *        MOVE MATCHING COST CONTAINMENT NAMES              *      ELGCBRI 
00627 *                                                          *      ELGCBRI 
00628 ************************************************************      ELGCBRI 
00629  MOVE-MATCHING-COST-CONTAINMENT.                                  ELGCBRI 
00630      ADD +1 TO TCAR-FROM-SUB.                                     ELGCBRI 
00631      MOVE WS-HOLD-LINE (WS-HOLD-SUB2) TO TCAR-FROM-LINE           ELGCBRI 
00632          (TCAR-FROM-SUB).                                         ELGCBRI 
00633      ADD +1 TO TCAR-FROM-SUB.                                     ELGCBRI 
00634      IF WS-HOLD-SUB2 < WS-HOLD-SUB                                ELGCBRI 
00635          PERFORM MOVE-AND-OR-BETWEEN-MULTIPLE-M                   ELGCBRI 
00636      ELSE                                                         ELGCBRI 
00637          PERFORM MOVE-COMMA-AFTER-LAST-MATCH.                     ELGCBRI 
00638                                                                   ELGCBRI 
00639                                                                   ELGCBRI 
00640 ************************************************************      ELGCBRI 
00641 *                                                          *      ELGCBRI 
00642 *        MOVE AND-OR BETWEEN MULTIPLE MATCHES              *      ELGCBRI 
00643 *                                                          *      ELGCBRI 
00644 ************************************************************      ELGCBRI 
00645  MOVE-AND-OR-BETWEEN-MULTIPLE-M.                                  ELGCBRI 
00646      MOVE PC-AND-OR TO TCAR-FROM-LINE (TCAR-FROM-SUB).            ELGCBRI 
00647                                                                   ELGCBRI 
00648                                                                   ELGCBRI 
00649 ************************************************************      ELGCBRI 
00650 *                                                          *      ELGCBRI 
00651 *        MOVE COMMA AFTER LAST MATCH                       *      ELGCBRI 
00652 *                                                          *      ELGCBRI 
00653 ************************************************************      ELGCBRI 
00654  MOVE-COMMA-AFTER-LAST-MATCH.                                     ELGCBRI 
00655      MOVE PC-COMMA TO TCAR-FROM-LINE (TCAR-FROM-SUB).             ELGCBRI 
00656      EJECT                                                        ELGCBRI 
00657                                                                   ELGCBRI 
00658                                                                   ELGCBRI 
00659 ************************************************************      ELGCBRI 
00660 *                                                          *      ELGCBRI 
00661 *        MOVE TRANSLATION TO COMPRESS AREA                 *      ELGCBRI 
00662 *                                                          *      ELGCBRI 
00663 ************************************************************      ELGCBRI 
00664  MOVE-TRANSLATION-TO-COMPRESS-A.                                  ELGCBRI 
00665      ADD  +1        TO  TCAR-FROM-SUB.                            ELGCBRI 
00666      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                          ELGCBRI 
00667                     TO  TCAR-FROM-LINE (TCAR-FROM-SUB).           ELGCBRI 
00668      EJECT                                                        ELGCBRI 
00669                                                                   ELGCBRI 
00670                                                                   ELGCBRI 
00671 ************************************************************      ELGCBRI 
00672 *                                                          *      ELGCBRI 
00673 *        MOVE FORMATTED TEXT TO OUTPUT AREA                *      ELGCBRI 
00674 *                                                          *      ELGCBRI 
00675 ************************************************************      ELGCBRI 
00676  MOVE-FORMATTED-TEXT-TO-OUTPUTX.                                  ELGCBRI 
00677      ADD  +1  TO  COF-NBR-DTL-LINES.                              ELGCBRI 
00678      MOVE TCAR-OPF-DATA (TCAR-X)                                  ELGCBRI 
00679               TO  COF-DTL-LINE (COF-NBR-DTL-LINES).               ELGCBRI 
00680      EJECT                                                        ELGCBRI 
00681                                                                   ELGCBRI 
00682                                                                   ELGCBRI 
00683 ************************************************************      ELGCBRI 
00684 *                                                          *      ELGCBRI 
00685 *        INITIALIZE COMPRESS AREA                          *      ELGCBRI 
00686 *                                                          *      ELGCBRI 
00687 ************************************************************      ELGCBRI 
00688  INITIALIZE-COMPRESS-AREA.                                        ELGCBRI 
00689      INITIALIZE TCAR-FROM-AREA                                    ELGCBRI 
00690                 TCAR-FROM-LENGTH                                  ELGCBRI 
00691                 TCAR-FROM-SUB.                                    ELGCBRI 
00692                                                                   ELGCBRI 
00693                                                                   ELGCBRI 
00694 ************************************************************      ELGCBRI 
00695 *                                                          *      ELGCBRI 
00696 *        SETUP FOR OUTPUT LINK                             *      ELGCBRI 
00697 *                                                          *      ELGCBRI 
00698 ************************************************************      ELGCBRI 
00699  SETUP-FOR-OUTPUT-LINK.                                           ELGCBRI 
00700      ADD  +1      TO  COF-NBR-DTL-LINES.                          ELGCBRI 
00701      MOVE SPACES  TO  COF-DTL-LINE (COF-NBR-DTL-LINES).           ELGCBRI 
00702      PERFORM LINK-TO-OUTPUT-MODULE.                               ELGCBRI 
00703      EJECT                                                        ELGCBRI 
00704                                                                   ELGCBRI 
00705                                                                   ELGCBRI 
00706 ************************************************************      ELGCBRI 
00707 *                                                          *      ELGCBRI 
00708 *        SETUP FOR UNSTRING OPERATION                      *      ELGCBRI 
00709 *                                                          *      ELGCBRI 
00710 ************************************************************      ELGCBRI 
00711  SETUP-FOR-UNSTRING-OPERATION.                                    ELGCBRI 
00712      MOVE +10  TO  TCAR-OUTPUT-FIELD-COUNT.                       ELGCBRI 
00713      MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                        ELGCBRI 
00714                    TCAR-OUTPUT-FIELD-2-LEN                        ELGCBRI 
00715                    TCAR-OUTPUT-FIELD-3-LEN                        ELGCBRI 
00716                    TCAR-OUTPUT-FIELD-4-LEN                        ELGCBRI 
00717                    TCAR-OUTPUT-FIELD-5-LEN                        ELGCBRI 
00718                    TCAR-OUTPUT-FIELD-6-LEN                        ELGCBRI 
00719                    TCAR-OUTPUT-FIELD-7-LEN                        ELGCBRI 
00720                    TCAR-OUTPUT-FIELD-8-LEN                        ELGCBRI 
00721                    TCAR-OUTPUT-FIELD-9-LEN                        ELGCBRI 
00722                    TCAR-OUTPUT-FIELD-10-LEN.                      ELGCBRI 
00723      EJECT                                                        ELGCBRI 
00724                                                                   ELGCBRI 
00725                                                                   ELGCBRI 
00726 ************************************************************      ELGCBRI 
00727 *                                                          *      ELGCBRI 
00728 *        CALL TEXT COMPRESSION MODULE                      *      ELGCBRI 
00729 *                                                          *      ELGCBRI 
00730 ************************************************************      ELGCBRI 
00731  CALL-TEXT-COMPRESSION-MODULE.                                    ELGCBRI 
00732      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGCBRI 
00733      EJECT                                                        ELGCBRI 
00734                                                                   ELGCBRI 
00735                                                                   ELGCBRI 
00736 ************************************************************      ELGCBRI 
00737 *                                                          *      ELGCBRI 
00738 *        CALL TEXT UNSTRING MODULE                         *      ELGCBRI 
00739 *                                                          *      ELGCBRI 
00740 ************************************************************      ELGCBRI 
00741  CALL-TEXT-UNSTRING-MODULE.                                       ELGCBRI 
00742      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGCBRI 
00743                                                                   ELGCBRI 
00744                                                                   ELGCBRI 
00745 ************************************************************      ELGCBRI 
00746 *                                                          *      ELGCBRI 
00747 *        LINK TO CODES MANUAL FOR TRANSLATION              *      ELGCBRI 
00748 *                                                          *      ELGCBRI 
00749 ************************************************************      ELGCBRI 
00750  LINK-TO-CODES-MANUAL-FOR-TRANS.                                  ELGCBRI 
00751      MOVE PC-GCCP TO  CMF-RECORD-PREFIX.                          ELGCBRI 
00752      EXEC CICS LINK PROGRAM ('ELUCMIF')                           ELGCBRI 
00753                     COMMAREA (DFHCOMMAREA)                        ELGCBRI 
00754                     LENGTH (LENGTH OF DFHCOMMAREA)                ELGCBRI 
00755                     END-EXEC.                                     ELGCBRI 
00756      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGCBRI 
00757      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCBRI 
00758                            ADDRESS OF CMF-DESCR.                  ELGCBRI 
00759      EJECT                                                        ELGCBRI 
00760                                                                   ELGCBRI 
00761                                                                   ELGCBRI 
00762 ************************************************************      ELGCBRI 
00763 *                                                          *      ELGCBRI 
00764 *        LINK TO OUTPUT MODULE                             *      ELGCBRI 
00765 *                                                          *      ELGCBRI 
00766 ************************************************************      ELGCBRI 
00767  LINK-TO-OUTPUT-MODULE.                                           ELGCBRI 
00768      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELGCBRI 
00769                     COMMAREA (DFHCOMMAREA)                        ELGCBRI 
00770                     LENGTH (LENGTH OF DFHCOMMAREA)                ELGCBRI 
00771                     END-EXEC.                                     ELGCBRI 
