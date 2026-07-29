00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELTCSMRY
00003  PROGRAM-ID.         ELTCSMRY.                                       LV002
00004                                                                   ELTCSMRY
00005  AUTHOR.             RICK BARILEAU.                               ELTCSMRY
00006                                                                   ELTCSMRY
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELTCSMRY
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELTCSMRY
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELTCSMRY
00010                      233 N. MICHIGAN AVE                          ELTCSMRY
00011                      CHICAGO, ILLINOIS 60601                      ELTCSMRY
00012                                                                   ELTCSMRY
00013  DATE-WRITTEN.       04-DEC-1987.                                 ELTCSMRY
00014                                                                   ELTCSMRY
00015  DATE-COMPILED.                                                   ELTCSMRY
00016                                                                   ELTCSMRY
00017  SECURITY.           COPYRIGHT 1987,                              ELTCSMRY
00018                      HEALTH CARE SERVICE CORPORATION              ELTCSMRY
00019      SKIP3                                                        ELTCSMRY
00020 ******************************************************************ELTCSMRY
00021 *                                                                *ELTCSMRY
00022 *    PROGRAM:    ELTCSMRY                                        *ELTCSMRY
00023 *    DATE:       04-DEC-1987                                     *ELTCSMRY
00024 *    AUTHOR:     RICK BARILEAU                                   *ELTCSMRY
00025 *    FUNCTION:   CONTRACT SUMMARY DRIVER PROGRAM                 *ELTCSMRY
00026 *                                                                *ELTCSMRY
00027 ******************************************************************ELTCSMRY
00028 *                                                                *ELTCSMRY
00029 *                      MAINTENANCE HISTORY                       *ELTCSMRY
00030 *                                                                *ELTCSMRY
00031 *  MOD     DATE     BY  DRPT                ACTION               *ELTCSMRY
00032 * ----- ----------- --- ----- ---------------------------------- *ELTCSMRY
00033 * 01.00 04-DEC-1987 REB       CREATED                            *ELTCSMRY
00034 *                                                                *ELTCSMRY
00035 * 01.01 10-DEC-1987 REB       INSERTED LINK TO 'ELUOUTPT' IN THE *ELTCSMRY
00036 *                             TERMINATION WITH THE RETURN.       *ELTCSMRY
00037 *                                                                *ELTCSMRY
00038 * 01.02 16-DEC-1987 REB       MOVED THE CICS RETURN IN OWN SECTIONELTCSMRY
00039 *                             COBOL COMPILER SQUAWKED ABOUT IT!  *ELTCSMRY
00040 *                                                                *ELTCSMRY
00041 * 01.03 13-JAN-1988 REB       ADDED CODE TO OBTAIN AND BUILD BP  *ELTCSMRY
00042 *                             REQUEST LIST FOR EACH SUBTOPIC.    *ELTCSMRY
00043 *                                                                *ELTCSMRY
00044 * 01.04 19-JAN-1988 REB       ADDED LOGIC TO HANDLE MULTIPLE     *ELTCSMRY
00045 *                             SELECTIONS BY USER. (NOTE: A NEW   *ELTCSMRY
00046 *                             AREA FOR THE NEW C.S. ACCUM        *ELTCSMRY
00047 *                             COPYBOOK WILL BE INITIALIZED ALSO) *ELTCSMRY
00048 *                                                                *ELTCSMRY
00049 * 01.05 29-JAN-1988 REB       OBTAIN AREA FOR ACCUMULATOR MATRIX *ELTCSMRY
00050 *                                                                *ELTCSMRY
00051 * 01.06 09-FEB-1988 REB       THIS WILL SET PROPER PROCESS SWITCH*ELTCSMRY
00052 *                             FOR EACH SUBTOPIC SELECTED.        *ELTCSMRY
00053 *                                                                *ELTCSMRY
00054 * 01.07 17-FEB-1988 REB       CHANGE LOGIC TO FILL ALL THE       *ELTCSMRY
00055 *                             NECESSARY BP PTRS THEN CALL COVERAGEELTCSMRY
00056 *                             AND REMOVED SETTING PROCESS SWITCH.*ELTCSMRY
00057 *                                                                *ELTCSMRY
00058 * 01.08 22-FEB-1988 REB       SETTING UP MASK LINE FOR ALL THE   *ELTCSMRY
00059 *                             SUBTOPICS.                         *ELTCSMRY
00060 *                                                                *ELTCSMRY
00061 * 01.09 26-FEB-1988 REB       INSERTING VALUE FOR CSPT-TBL-CNT   *ELTCSMRY
00062 *                             PER NINA.                          *ELTCSMRY
00063 *                                                                *ELTCSMRY
00064 * 01.10 02-MAR-1988 REB       NINA WANTS DRIVER TO PULL IN ALL   *ELTCSMRY
00065 *                             ACCUMS THAT DEAL WITH GROUP        *ELTCSMRY
00066 *                             INCLUDING : #ABM,#ACL,#ADL,#AOL.   *ELTCSMRY
00067 *                                                                *ELTCSMRY
00068 * 01.11 03-MAR-1988 REB       NINA WOULD LIKE DRIVER TO DISPLAY  *ELTCSMRY
00069 *                             THE DISCLAIMER MESSAGE AFTER EACH  *ELTCSMRY
00070 *                             SUBTOPIC.                          *ELTCSMRY
00071 *                                                                *ELTCSMRY
00072 * 01.12 09-MAR-1988 REB       THE SELECTIONS ARE IN A NEW TABLE  *ELTCSMRY
00073 *                             'SSB-CS-MNU-RESPONSE-TABLE'.       *ELTCSMRY
00074 *                                                                *ELTCSMRY
00075 * 01.13 29-MAR-1988 AKK       ADDED CODE TO DETERMINE IF PROCES- *ELTCSMRY
00076 *                             A MEDICARE CONTRACT.  IF SO, THE   *ELTCSMRY
00077 *                             IHM PARAMETERS ARE LOADED INSTEAD  *ELTCSMRY
00078 *                             OF THE IHS PARAMETERS.             *ELTCSMRY
00079 *                                                                *ELTCSMRY
00080 * 01.14 15-APR-1988 AKK       PRODUCTION DISCREPANCY - DUE       *ELTCSMRY
00081 *                             TO ASRA ON SELECTING MEDICARE      *ELTCSMRY
00082 *                             ELIGIBLE AND THE IHS TOPIC ON      *ELTCSMRY
00083 *                             CONTRACT SUMMARY.                  *ELTCSMRY
00084 *                                                                *ELTCSMRY
00085 * 02.00 25-JUL-1988 AKK       REMOVED CODE FOR BUILDING ELSCSACC *ELTCSMRY
00086 *                             AS IT WILL BE PLACED IN A MODULE   *ELTCSMRY
00087 *                             ELUGCLDA.  ALSO ADDED A LINK TO    *ELTCSMRY
00088 *                             ELUGCLDA REGARDLESS OF USER SEL-   *ELTCSMRY
00089 *                             ECTION AND A LINK TO ELUOVCFD      *ELTCSMRY
00090 *                             UNLESS CCP ONLY SELECTED.           ELTCSMRY
00091 * 02.01 15-SEP-1988 NAC       ALLOW LINK TO ELUOVCFD TO PROCESS  *ELTCSMRY
00092 * 02.02 13-OCT-1988 NAC       CHANGE VERBIAGE DISPLAYED AT THE   *ELTCSMRY
00093 *                             OF ALL SUBTOPICS.  MOVE ORIGINAL   *ELTCSMRY
00094 *                             VERBIAGE SHOWN AT SUBTOPIC LEVEL TO*ELTCSMRY
00095 *                             CONTRACT SUMMARY SUBTOPIC.         *ELTCSMRY
00096 * 02.03 17-OCT-1988 NAC       CORRECT LOGIC WHICH DETERMINES IF  *ELTCSMRY
00097 *                             CCP WAS THE ONLY SUBTOPIC SELECTED;*ELTCSMRY
00098 *                             WHEN 'CC' IS ENTERED AS THE LAST   *ELTCSMRY
00099 *                             SUBTOPIC OF MULTIPLE SUBTOPICS, IT *ELTCSMRY
00100 *                             ASSUMED THAT IT WAS THE ONLY SUB-  *ELTCSMRY
00101 *                             TOPIC ENTERED.                     *ELTCSMRY
00102 * 02.03 21-DEC-1989 AKK       DESTRUCTED.                        *ELTCSMRY
00103 *                                                                *ELTCSMRY
00104 * 02.04 30-SEP-1991 JPB       CHANGED WS-M TO PIC XX FOR FAMILY  *ELTCSMRY
00105 *                             RELATIONSHIP EXPANSION.            *ELTCSMRY
00106 *                                                                *ELTCSMRY
00107 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTCSMRY
00108 *                                                                *ELTCSMRY
00109 ******************************************************************ELTCSMRY
00110      EJECT                                                        ELTCSMRY
00111  ENVIRONMENT DIVISION.                                            ELTCSMRY
00112                                                                   ELTCSMRY
00113  CONFIGURATION SECTION.                                           ELTCSMRY
00114  SOURCE-COMPUTER.    IBM-3033.                                    ELTCSMRY
00115  OBJECT-COMPUTER.    IBM-3033.                                    ELTCSMRY
00116      EJECT                                                        ELTCSMRY
00117  DATA DIVISION.                                                   ELTCSMRY
00118                                                                   ELTCSMRY
00119  FILE SECTION.                                                    ELTCSMRY
00120                                                                   ELTCSMRY
00121  WORKING-STORAGE SECTION.                                         ELTCSMRY
00122  01  SWITCH.                                                      ELTCSMRY
00123      05  WS-SELECT-SWITCH       PIC X(01)     VALUE SPACE.        ELTCSMRY
00124          88  ALL-NOT-FOUND                    VALUE 'N'.          ELTCSMRY
00125          88  ALL-FOUND                        VALUE 'Y'.          ELTCSMRY
00126                                                                   ELTCSMRY
00127      05  WS-BP-SUBTOPIC-SW      PIC X(01)     VALUE 'N'.          ELTCSMRY
00128          88  BP-SUBTOPIC-SELECTED             VALUE 'B'.          ELTCSMRY
00129                                                                   ELTCSMRY
00130      05  WS-FIRST-TIME-SW       PIC X(01)     VALUE SPACE.        ELTCSMRY
00131          88  FIRST-TIME                       VALUE 'Y'.          ELTCSMRY
00132                                                                   ELTCSMRY
00133      05  CCP-ONLY-SWITCH        PIC X(01)     VALUE SPACE.        ELTCSMRY
00134          88  CCP-ONLY-FOUND                   VALUE 'C'.          ELTCSMRY
00135          88  NOT-CCP-ONLY-FOUND               VALUE 'N'.          ELTCSMRY
00136                                                                   ELTCSMRY
00137  01  WS-MISC.                                                     ELTCSMRY
00138      05  WS-MODULE-NAME         PIC X(08).                        ELTCSMRY
00139          88  CCP                              VALUE 'ELGCSCCP'.   ELTCSMRY
00140          88  IHS                              VALUE 'ELGCSIHS'.   ELTCSMRY
00141          88  IPS                              VALUE 'ELGCSIPS'.   ELTCSMRY
00142          88  OPS                              VALUE 'ELGCSOPS'.   ELTCSMRY
00143          88  OBS                              VALUE 'ELGCSOBS'.   ELTCSMRY
00144          88  PSY                              VALUE 'ELGCSPSY'.   ELTCSMRY
00145          88  GCI                              VALUE 'ELGCSGCI'.   ELTCSMRY
00146                                                                   ELTCSMRY
00147      05  WS-FIVE                PIC 9(01)     VALUE 5.            ELTCSMRY
00148      05  WS-FOUR                PIC 9(01)     VALUE 4.            ELTCSMRY
00149      05  WS-MASK-LINE.                                            ELTCSMRY
00150          10  FILLER             PIC X(33)     VALUE               ELTCSMRY
00151          '                              |  '.                     ELTCSMRY
00152          10  FILLER             PIC X(46)     VALUE SPACES.       ELTCSMRY
00153      05  WS-NBR-CHOICES         PIC S9(04)    VALUE +0000 COMP.   ELTCSMRY
00154                                                                   ELTCSMRY
00155      05  WS-DASH-LINE.                                            ELTCSMRY
00156          10  FILLER             PIC X(70)     VALUE '-------------ELTCSMRY
00157 -    '---------------------------------------------------------'. ELTCSMRY
00158          10  FILLER             PIC X(09)     VALUE '---------'.  ELTCSMRY
00159      05  WS-DISCLAIMER-1.                                         ELTCSMRY
00160          10  FILLER             PIC X(70)     VALUE '******  THIS ELTCSMRY
00161 -    'CONTRACT SUMMARY DISPLAYS LIMITED INFORMATION ONLY. CHECK'. ELTCSMRY
00162          10  FILLER             PIC X(09)     VALUE '  ******'.   ELTCSMRY
00163      05  WS-DISCLAIMER-2.                                         ELTCSMRY
00164          10  FILLER             PIC X(71)     VALUE '******  INDIVELTCSMRY
00165 -    'IDUAL TOPIC FOR ANY ADDITIONAL INFORMATION THAT MAY APPLY.'.ELTCSMRY
00166          10  FILLER             PIC X(08)     VALUE ' ******'.    ELTCSMRY
00167                                                                   ELTCSMRY
00168      05  WS-DISCLAIMER-3.                                         ELTCSMRY
00169          10  FILLER             PIC X(70)     VALUE '******  SEE GELTCSMRY
00170 -    'ENERAL CONTRACT SUBTOPIC FOR OVERALL COINSURANCE, MAXIMUM'. ELTCSMRY
00171          10  FILLER             PIC X(09)     VALUE '  ******'.   ELTCSMRY
00172      05  WS-DISCLAIMER-4.                                         ELTCSMRY
00173          10  FILLER             PIC X(71)     VALUE '******  AND DELTCSMRY
00174 -    'EDUCTIBLE THAT MAY APPLY.                                 '.ELTCSMRY
00175          10  FILLER             PIC X(08)     VALUE ' ******'.    ELTCSMRY
00176                                                                   ELTCSMRY
00177  01  PROGRAM-CONSTANTS.                                           ELTCSMRY
00178      05  PC-ABM                 PIC X(06)     VALUE '#ABM  '.     ELTCSMRY
00179      05  PC-ACL                 PIC X(06)     VALUE '#ACL  '.     ELTCSMRY
00180      05  PC-ADL                 PIC X(06)     VALUE '#ADL  '.     ELTCSMRY
00181      05  PC-ALL                 PIC X(16)     VALUE 'ALL'.        ELTCSMRY
00182      05  PC-AOL                 PIC X(06)     VALUE '#AOL  '.     ELTCSMRY
00183      05  PC-FAM-REL-LVL-MEDIC   PIC XX        VALUE '0M'.         ELTCSMRY
00184                                                                   ELTCSMRY
00185  01  CS-SELECTION-AREA.                                           ELTCSMRY
00186      05  CS-USER-SELECTION.                                       ELTCSMRY
00187          10  FILLER             PIC X(24)     VALUE               ELTCSMRY
00188              'CC              ELGCSCCP'.                          ELTCSMRY
00189          10  FILLER             PIC X(24)     VALUE               ELTCSMRY
00190              'IPHB            ELGCSIHS'.                          ELTCSMRY
00191          10  FILLER             PIC X(24)     VALUE               ELTCSMRY
00192              'MD              ELGCSIPS'.                          ELTCSMRY
00193          10  FILLER             PIC X(24)     VALUE               ELTCSMRY
00194              'OUT             ELGCSOPS'.                          ELTCSMRY
00195          10  FILLER             PIC X(24)     VALUE               ELTCSMRY
00196              'OBS             ELGCSOBS'.                          ELTCSMRY
00197          10  FILLER             PIC X(24)     VALUE               ELTCSMRY
00198              'MENT            ELGCSPSY'.                          ELTCSMRY
00199          10  FILLER             PIC X(24)     VALUE               ELTCSMRY
00200              'GCI             ELGCSGCI'.                          ELTCSMRY
00201          10  FILLER             PIC X(24)     VALUE               ELTCSMRY
00202              'ALL             ELGCSXXX'.                          ELTCSMRY
00203 *                                                                 ELTCSMRY
00204      05  CS-SELECTION-INFO    REDEFINES CS-USER-SELECTION         ELTCSMRY
00205                               OCCURS 8 TIMES                      ELTCSMRY
00206                               INDEXED BY CS-IDX.                  ELTCSMRY
00207          10  CS-KEYWORD       PIC X(16).                          ELTCSMRY
00208          10  CS-MODULE-NAME   PIC X(08).                          ELTCSMRY
00209                                                                   ELTCSMRY
00210 ***** THIS COPYBOOK 'ELSCSTPC' WILL CONTAIN THE PARAMETER LIST    ELTCSMRY
00211 ***** FOR EACH CONTRACT SUMMARY SUBTOPIC.                         ELTCSMRY
00212 /                                                                 ELTCSMRY
00213  COPY ELSCSTPC.                                                   ELTCSMRY
00214                                                                   ELTCSMRY
00215 /                                                                 ELTCSMRY
00216  LINKAGE SECTION.                                                 ELTCSMRY
00217  01  DFHCOMMAREA.                                                 ELTCSMRY
00218      COPY ELSCOMMC.                                               ELTCSMRY
00219 /                                                                 ELTCSMRY
00220      COPY ELSCIA2C.                                               ELTCSMRY
00221 /                                                                 ELTCSMRY
00222      COPY ELSSSCBC.                                               ELTCSMRY
00223 /                                                                 ELTCSMRY
00224      COPY ELSIOPMC.                                               ELTCSMRY
00225 /                                                                 ELTCSMRY
00226      COPY ELSKEYSC.                                               ELTCSMRY
00227 /                                                                 ELTCSMRY
00228      COPY ELSOUTPC.                                               ELTCSMRY
00229 /                                                                 ELTCSMRY
00230      COPY ELSCSPTC.                                               ELTCSMRY
00231 /                                                                 ELTCSMRY
00232  01  GROUP-SPECIFIC-RECORD.                                       ELTCSMRY
00233      COPY GCGROUPC.                                               ELTCSMRY
00234 /                                                                 ELTCSMRY
00235  01  CONTRACT-RECORD.                                             ELTCSMRY
00236      COPY GCCONTRC.                                               ELTCSMRY
00237      EJECT                                                        ELTCSMRY
00238  PROCEDURE DIVISION.                                              ELTCSMRY
00239 ************************************************************      ELTCSMRY
00240 *                                                          *      ELTCSMRY
00241 *                    PROCEDURE DIVISION                    *      ELTCSMRY
00242 *                                                          *      ELTCSMRY
00243 ************************************************************      ELTCSMRY
00244                                                                   ELTCSMRY
00245                                                                   ELTCSMRY
00246 ************************************************************      ELTCSMRY
00247 *                                                          *      ELTCSMRY
00248 *        PERFORM CONTRACT SUMMARY DRIVER FUNCTION          *      ELTCSMRY
00249 *                                                          *      ELTCSMRY
00250 ************************************************************      ELTCSMRY
00251  PERFORM-CONTRACT-SUMMARY-DRIVE.                                  ELTCSMRY
00252      PERFORM INITIALIZATION.                                      ELTCSMRY
00253      PERFORM PROCESS.                                             ELTCSMRY
00254      GOBACK.                                                      ELTCSMRY
00255                                                                   ELTCSMRY
00256                                                                   ELTCSMRY
00257 ************************************************************      ELTCSMRY
00258 *                                                          *      ELTCSMRY
00259 *        INITIALIZATION                                    *      ELTCSMRY
00260 *                                                          *      ELTCSMRY
00261 ************************************************************      ELTCSMRY
00262  INITIALIZATION.                                                  ELTCSMRY
00263      PERFORM CHECK-COMMAREA-LENGTH.                               ELTCSMRY
00264      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELTCSMRY
00265      PERFORM ESTABLISH-ADDRESSING-TO-SELECT.                      ELTCSMRY
00266      PERFORM ESTABLISH-ADDRESSING-TO-WORK-A.                      ELTCSMRY
00267      EJECT                                                        ELTCSMRY
00268                                                                   ELTCSMRY
00269                                                                   ELTCSMRY
00270 ************************************************************      ELTCSMRY
00271 *                                                          *      ELTCSMRY
00272 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELTCSMRY
00273 *                                                          *      ELTCSMRY
00274 ************************************************************      ELTCSMRY
00275  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELTCSMRY
00276      IF ECA-CIA-PTR IS NOT EQUAL NULL                             ELTCSMRY
00277          PERFORM SET-CIA-ADDRESS                                  ELTCSMRY
00278      ELSE                                                         ELTCSMRY
00279          PERFORM SIGNAL-CIA-ADDRESSING-ERROR.                     ELTCSMRY
00280                                                                   ELTCSMRY
00281                                                                   ELTCSMRY
00282 ************************************************************      ELTCSMRY
00283 *                                                          *      ELTCSMRY
00284 *        CHECK COMMAREA LENGTH                             *      ELTCSMRY
00285 *                                                          *      ELTCSMRY
00286 ************************************************************      ELTCSMRY
00287  CHECK-COMMAREA-LENGTH.                                           ELTCSMRY
00288      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELTCSMRY
00289          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELTCSMRY
00290                                                                   ELTCSMRY
00291                                                                   ELTCSMRY
00292 ************************************************************      ELTCSMRY
00293 *                                                          *      ELTCSMRY
00294 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELTCSMRY
00295 *                                                          *      ELTCSMRY
00296 ************************************************************      ELTCSMRY
00297  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELTCSMRY
00298      EXEC CICS ABEND                                              ELTCSMRY
00299                ABCODE('EL01')                                     ELTCSMRY
00300                END-EXEC.                                          ELTCSMRY
00301                                                                   ELTCSMRY
00302                                                                   ELTCSMRY
00303 ************************************************************      ELTCSMRY
00304 *                                                          *      ELTCSMRY
00305 *        SET CIA ADDRESS                                   *      ELTCSMRY
00306 *                                                          *      ELTCSMRY
00307 ************************************************************      ELTCSMRY
00308  SET-CIA-ADDRESS.                                                 ELTCSMRY
00309      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCSMRY
00310           ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.               ELTCSMRY
00311                                                                   ELTCSMRY
00312                                                                   ELTCSMRY
00313 ************************************************************      ELTCSMRY
00314 *                                                          *      ELTCSMRY
00315 *        SIGNAL CIA ADDRESSING ERROR                       *      ELTCSMRY
00316 *                                                          *      ELTCSMRY
00317 ************************************************************      ELTCSMRY
00318  SIGNAL-CIA-ADDRESSING-ERROR.                                     ELTCSMRY
00319      EXEC CICS ABEND                                              ELTCSMRY
00320                ABCODE('EL02')                                     ELTCSMRY
00321                END-EXEC.                                          ELTCSMRY
00322      EJECT                                                        ELTCSMRY
00323                                                                   ELTCSMRY
00324                                                                   ELTCSMRY
00325 ************************************************************      ELTCSMRY
00326 *                                                          *      ELTCSMRY
00327 *        ESTABLISH ADDRESSING TO SELECTOR CONTROL AREA     *      ELTCSMRY
00328 *                                                          *      ELTCSMRY
00329 ************************************************************      ELTCSMRY
00330  ESTABLISH-ADDRESSING-TO-SELECT.                                  ELTCSMRY
00331      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCSMRY
00332      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCSMRY
00333           ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                 ELTCSMRY
00334      IF CIA-RC-PTR-NULL                                           ELTCSMRY
00335          PERFORM SIGNAL-MISSING-PARAMETER.                        ELTCSMRY
00336                                                                   ELTCSMRY
00337                                                                   ELTCSMRY
00338 ************************************************************      ELTCSMRY
00339 *                                                          *      ELTCSMRY
00340 *        SIGNAL MISSING PARAMETER                          *      ELTCSMRY
00341 *                                                          *      ELTCSMRY
00342 ************************************************************      ELTCSMRY
00343  SIGNAL-MISSING-PARAMETER.                                        ELTCSMRY
00344      SET CIA-AB-PARM-MISSING TO TRUE.                             ELTCSMRY
00345      PERFORM SIGNAL-ABEND.                                        ELTCSMRY
00346                                                                   ELTCSMRY
00347                                                                   ELTCSMRY
00348 ************************************************************      ELTCSMRY
00349 *                                                          *      ELTCSMRY
00350 *        SIGNAL ABEND                                      *      ELTCSMRY
00351 *                                                          *      ELTCSMRY
00352 ************************************************************      ELTCSMRY
00353  SIGNAL-ABEND.                                                    ELTCSMRY
00354      EXEC CICS ABEND                                              ELTCSMRY
00355                ABCODE (CIA-ABCODE)                                ELTCSMRY
00356         END-EXEC.                                                 ELTCSMRY
00357      EJECT                                                        ELTCSMRY
00358                                                                   ELTCSMRY
00359                                                                   ELTCSMRY
00360 ************************************************************      ELTCSMRY
00361 *                                                          *      ELTCSMRY
00362 *        ESTABLISH ADDRESSING TO WORK AREAS                *      ELTCSMRY
00363 *                                                          *      ELTCSMRY
00364 ************************************************************      ELTCSMRY
00365  ESTABLISH-ADDRESSING-TO-WORK-A.                                  ELTCSMRY
00366      PERFORM ESTABLISH-ADDRESSING-TO-KEY-WO.                      ELTCSMRY
00367      PERFORM ESTABLISH-ADDRESSING-TO-OUTPUT.                      ELTCSMRY
00368      PERFORM ESTABLISH-ADDRESSING-TO-GROUPX.                      ELTCSMRY
00369                                                                   ELTCSMRY
00370                                                                   ELTCSMRY
00371 ************************************************************      ELTCSMRY
00372 *                                                          *      ELTCSMRY
00373 *        ESTABLISH ADDRESSING TO KEY WORK AREA             *      ELTCSMRY
00374 *                                                          *      ELTCSMRY
00375 ************************************************************      ELTCSMRY
00376  ESTABLISH-ADDRESSING-TO-KEY-WO.                                  ELTCSMRY
00377      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCSMRY
00378      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCSMRY
00379           ADDRESS OF KWA-FILE-KEY-WORK-AREA.                      ELTCSMRY
00380      IF CIA-RC-PTR-NULL                                           ELTCSMRY
00381          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCSMRY
00382                                                                   ELTCSMRY
00383                                                                   ELTCSMRY
00384 ************************************************************      ELTCSMRY
00385 *                                                          *      ELTCSMRY
00386 *        ESTABLISH ADDRESSING TO OUTPUT INTERFACE AREA     *      ELTCSMRY
00387 *                                                          *      ELTCSMRY
00388 ************************************************************      ELTCSMRY
00389  ESTABLISH-ADDRESSING-TO-OUTPUT.                                  ELTCSMRY
00390      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCSMRY
00391      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCSMRY
00392           ADDRESS OF COF-OUTPUT-INTERFACE.                        ELTCSMRY
00393      IF CIA-RC-PTR-NULL                                           ELTCSMRY
00394          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCSMRY
00395      EJECT                                                        ELTCSMRY
00396                                                                   ELTCSMRY
00397                                                                   ELTCSMRY
00398 ************************************************************      ELTCSMRY
00399 *                                                          *      ELTCSMRY
00400 *        ESTABLISH ADDRESSING TO GROUP SPECIFIC RECORD AREA*      ELTCSMRY
00401 *                                                          *      ELTCSMRY
00402 ************************************************************      ELTCSMRY
00403  ESTABLISH-ADDRESSING-TO-GROUPX.                                  ELTCSMRY
00404      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTCSMRY
00405      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCSMRY
00406           ADDRESS OF GROUP-SPECIFIC-RECORD.                       ELTCSMRY
00407      IF CIA-RC-PTR-NULL                                           ELTCSMRY
00408          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELTCSMRY
00409                                                                   ELTCSMRY
00410                                                                   ELTCSMRY
00411 ************************************************************      ELTCSMRY
00412 *                                                          *      ELTCSMRY
00413 *        SIGNAL UNALLOC AREA ERROR                         *      ELTCSMRY
00414 *                                                          *      ELTCSMRY
00415 ************************************************************      ELTCSMRY
00416  SIGNAL-UNALLOC-AREA-ERROR.                                       ELTCSMRY
00417      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTCSMRY
00418      PERFORM SIGNAL-ABEND.                                        ELTCSMRY
00419      EJECT                                                        ELTCSMRY
00420                                                                   ELTCSMRY
00421                                                                   ELTCSMRY
00422 ************************************************************      ELTCSMRY
00423 *                                                          *      ELTCSMRY
00424 *        PROCESS                                           *      ELTCSMRY
00425 *                                                          *      ELTCSMRY
00426 ************************************************************      ELTCSMRY
00427  PROCESS.                                                         ELTCSMRY
00428      PERFORM LINK-TO-ACCUM-LOADER.                                ELTCSMRY
00429      MOVE WS-MASK-LINE TO COF-MASK-LINE.                          ELTCSMRY
00430      SET NOT-CCP-ONLY-FOUND TO TRUE.                              ELTCSMRY
00431      PERFORM DETERMINE-IF-ALL-WAS-SELECTED.                       ELTCSMRY
00432      IF ALL-NOT-FOUND                                             ELTCSMRY
00433          PERFORM DETERMINE-WHAT-USER-SELECTED.                    ELTCSMRY
00434      IF NOT-CCP-ONLY-FOUND                                        ELTCSMRY
00435          PERFORM LINK-TO-ELUOVCFD.                                ELTCSMRY
00436      IF BP-SUBTOPIC-SELECTED                                      ELTCSMRY
00437          PERFORM CALL-COVERAGE-PROGRAM.                           ELTCSMRY
00438      IF ALL-FOUND                                                 ELTCSMRY
00439          PERFORM DISPLAY-EVERYTHING-FOR-THE-USE                   ELTCSMRY
00440      ELSE                                                         ELTCSMRY
00441          PERFORM DISPLAY-ONLY-INFORMATION-USERX.                  ELTCSMRY
00442      PERFORM TERMINATE-OUTPUT-DISPLAY.                            ELTCSMRY
00443      EJECT                                                        ELTCSMRY
00444                                                                   ELTCSMRY
00445                                                                   ELTCSMRY
00446 ************************************************************      ELTCSMRY
00447 *                                                          *      ELTCSMRY
00448 *        LINK TO ACCUM LOADER                              *      ELTCSMRY
00449 *                                                          *      ELTCSMRY
00450 ************************************************************      ELTCSMRY
00451  LINK-TO-ACCUM-LOADER.                                            ELTCSMRY
00452      EXEC CICS LINK                                               ELTCSMRY
00453                PROGRAM ('ELUGCLDA')                               ELTCSMRY
00454                COMMAREA (DFHCOMMAREA)                             ELTCSMRY
00455                END-EXEC.                                          ELTCSMRY
00456                                                                   ELTCSMRY
00457                                                                   ELTCSMRY
00458 ************************************************************      ELTCSMRY
00459 *                                                          *      ELTCSMRY
00460 *        CALL STORAGE MANAGER                              *      ELTCSMRY
00461 *                                                          *      ELTCSMRY
00462 ************************************************************      ELTCSMRY
00463  CALL-STORAGE-MANAGER.                                            ELTCSMRY
00464      EXEC CICS LINK                                               ELTCSMRY
00465                PROGRAM ('ELUSTGMG')                               ELTCSMRY
00466                COMMAREA (DFHCOMMAREA)                             ELTCSMRY
00467          END-EXEC.                                                ELTCSMRY
00468      EJECT                                                        ELTCSMRY
00469                                                                   ELTCSMRY
00470                                                                   ELTCSMRY
00471 ************************************************************      ELTCSMRY
00472 *                                                          *      ELTCSMRY
00473 *        DETERMINE IF ALL WAS SELECTED                     *      ELTCSMRY
00474 *                                                          *      ELTCSMRY
00475 ************************************************************      ELTCSMRY
00476  DETERMINE-IF-ALL-WAS-SELECTED.                                   ELTCSMRY
00477      SET SSB-CS-RESP-IDX TO +1.                                   ELTCSMRY
00478      SEARCH SSB-CS-RESPONSE                                       ELTCSMRY
00479           AT END                                                  ELTCSMRY
00480                SET ALL-NOT-FOUND         TO TRUE                  ELTCSMRY
00481           WHEN SSB-CS-RESPONSE (SSB-CS-RESP-IDX) =                ELTCSMRY
00482          PC-ALL                                                   ELTCSMRY
00483                SET ALL-FOUND             TO TRUE                  ELTCSMRY
00484                SET BP-SUBTOPIC-SELECTED  TO TRUE                  ELTCSMRY
00485         END-SEARCH.                                               ELTCSMRY
00486      IF ALL-FOUND                                                 ELTCSMRY
00487          PERFORM SETUP-BP-POINTER-FOR-SUBTOPIC.                   ELTCSMRY
00488      EJECT                                                        ELTCSMRY
00489                                                                   ELTCSMRY
00490                                                                   ELTCSMRY
00491 ************************************************************      ELTCSMRY
00492 *                                                          *      ELTCSMRY
00493 *        DETERMINE WHAT USER SELECTED                      *      ELTCSMRY
00494 *                                                          *      ELTCSMRY
00495 ************************************************************      ELTCSMRY
00496  DETERMINE-WHAT-USER-SELECTED.                                    ELTCSMRY
00497      PERFORM DETERMINE-WHICH-SUBTOPIC-TO-BU                       ELTCSMRY
00498          VARYING SSB-CS-RESP-IDX FROM +1 BY +1                    ELTCSMRY
00499                 UNTIL   SSB-CS-RESP-IDX > +10                     ELTCSMRY
00500                 OR      SSB-CS-RESPONSE (SSB-CS-RESP-IDX) =       ELTCSMRY
00501              SPACES.                                              ELTCSMRY
00502      PERFORM DETERMINE-IF-CCP-ONLY-SELECTED.                      ELTCSMRY
00503                                                                   ELTCSMRY
00504                                                                   ELTCSMRY
00505 ************************************************************      ELTCSMRY
00506 *                                                          *      ELTCSMRY
00507 *        DETERMINE WHICH SUBTOPIC TO BUILD REQUEST LIST FOR*      ELTCSMRY
00508 *                                                          *      ELTCSMRY
00509 ************************************************************      ELTCSMRY
00510  DETERMINE-WHICH-SUBTOPIC-TO-BU.                                  ELTCSMRY
00511      SET CS-IDX TO +1.                                            ELTCSMRY
00512      SEARCH CS-SELECTION-INFO                                     ELTCSMRY
00513           AT END                                                  ELTCSMRY
00514              SET CIA-AB-PARM-ERR TO TRUE                          ELTCSMRY
00515              EXEC CICS ABEND  ABCODE (CIA-ABCODE)                 ELTCSMRY
00516                   END-EXEC                                        ELTCSMRY
00517           WHEN SSB-CS-RESPONSE (SSB-CS-RESP-IDX) = CS-KEYWORD     ELTCSMRY
00518          (CS-IDX)                                                 ELTCSMRY
00519                  MOVE CS-MODULE-NAME (CS-IDX) TO                  ELTCSMRY
00520          WS-MODULE-NAME                                           ELTCSMRY
00521         END-SEARCH.                                               ELTCSMRY
00522      ADD +1               TO WS-NBR-CHOICES.                      ELTCSMRY
00523      IF (NOT GCI) AND (NOT CCP)                                   ELTCSMRY
00524          PERFORM SETUP-BP-POINTER-FOR-SUBTOPIC.                   ELTCSMRY
00525                                                                   ELTCSMRY
00526                                                                   ELTCSMRY
00527 ************************************************************      ELTCSMRY
00528 *                                                          *      ELTCSMRY
00529 *        DETERMINE IF CCP ONLY SELECTED                    *      ELTCSMRY
00530 *                                                          *      ELTCSMRY
00531 ************************************************************      ELTCSMRY
00532  DETERMINE-IF-CCP-ONLY-SELECTED.                                  ELTCSMRY
00533      IF SSB-CS-RESPONSE (1) = 'CC' AND WS-NBR-CHOICES =           ELTCSMRY
00534          1                                                        ELTCSMRY
00535          PERFORM SET-CCP-ONLY-SWITCH.                             ELTCSMRY
00536      EJECT                                                        ELTCSMRY
00537                                                                   ELTCSMRY
00538                                                                   ELTCSMRY
00539 ************************************************************      ELTCSMRY
00540 *                                                          *      ELTCSMRY
00541 *        LINK TO ELUOVCFD                                  *      ELTCSMRY
00542 *                                                          *      ELTCSMRY
00543 ************************************************************      ELTCSMRY
00544  LINK-TO-ELUOVCFD.                                                ELTCSMRY
00545      EXEC CICS LINK                                               ELTCSMRY
00546                PROGRAM ('ELUOVCFD')                               ELTCSMRY
00547                COMMAREA (DFHCOMMAREA)                             ELTCSMRY
00548                END-EXEC.                                          ELTCSMRY
00549                                                                   ELTCSMRY
00550                                                                   ELTCSMRY
00551 ************************************************************      ELTCSMRY
00552 *                                                          *      ELTCSMRY
00553 *        SET CCP ONLY SWITCH                               *      ELTCSMRY
00554 *                                                          *      ELTCSMRY
00555 ************************************************************      ELTCSMRY
00556  SET-CCP-ONLY-SWITCH.                                             ELTCSMRY
00557      SET CCP-ONLY-FOUND TO TRUE.                                  ELTCSMRY
00558      EJECT                                                        ELTCSMRY
00559                                                                   ELTCSMRY
00560                                                                   ELTCSMRY
00561 ************************************************************      ELTCSMRY
00562 *                                                          *      ELTCSMRY
00563 *        DISPLAY EVERYTHING FOR THE USER                   *      ELTCSMRY
00564 *                                                          *      ELTCSMRY
00565 ************************************************************      ELTCSMRY
00566  DISPLAY-EVERYTHING-FOR-THE-USE.                                  ELTCSMRY
00567      PERFORM CALL-REQUESTED-PROGRAM                               ELTCSMRY
00568          VARYING CS-IDX FROM +1 BY +1                             ELTCSMRY
00569                 UNTIL   CS-IDX > +7.                              ELTCSMRY
00570      EJECT                                                        ELTCSMRY
00571                                                                   ELTCSMRY
00572                                                                   ELTCSMRY
00573 ************************************************************      ELTCSMRY
00574 *                                                          *      ELTCSMRY
00575 *        DISPLAY ONLY INFORMATION USER REQUESTED           *      ELTCSMRY
00576 *                                                          *      ELTCSMRY
00577 ************************************************************      ELTCSMRY
00578  DISPLAY-ONLY-INFORMATION-USERX.                                  ELTCSMRY
00579      PERFORM DETERMINE-SUBTOPIC-AND-CALL-PR                       ELTCSMRY
00580          VARYING SSB-CS-RESP-IDX FROM +1 BY +1                    ELTCSMRY
00581                 UNTIL   SSB-CS-RESP-IDX > WS-NBR-CHOICES.         ELTCSMRY
00582                                                                   ELTCSMRY
00583                                                                   ELTCSMRY
00584 ************************************************************      ELTCSMRY
00585 *                                                          *      ELTCSMRY
00586 *        DETERMINE SUBTOPIC AND CALL PROGRAM               *      ELTCSMRY
00587 *                                                          *      ELTCSMRY
00588 ************************************************************      ELTCSMRY
00589  DETERMINE-SUBTOPIC-AND-CALL-PR.                                  ELTCSMRY
00590      SET CS-IDX TO +1.                                            ELTCSMRY
00591      SEARCH CS-SELECTION-INFO                                     ELTCSMRY
00592           AT END                                                  ELTCSMRY
00593              SET CIA-AB-PARM-ERR TO TRUE                          ELTCSMRY
00594              EXEC CICS ABEND  ABCODE (CIA-ABCODE)                 ELTCSMRY
00595                   END-EXEC                                        ELTCSMRY
00596           WHEN SSB-CS-RESPONSE (SSB-CS-RESP-IDX) = CS-KEYWORD     ELTCSMRY
00597          (CS-IDX)                                                 ELTCSMRY
00598                CONTINUE                                           ELTCSMRY
00599         END-SEARCH.                                               ELTCSMRY
00600      PERFORM CALL-REQUESTED-PROGRAM.                              ELTCSMRY
00601      EJECT                                                        ELTCSMRY
00602                                                                   ELTCSMRY
00603                                                                   ELTCSMRY
00604 ************************************************************      ELTCSMRY
00605 *                                                          *      ELTCSMRY
00606 *        SETUP BP POINTER FOR SUBTOPIC                     *      ELTCSMRY
00607 *                                                          *      ELTCSMRY
00608 ************************************************************      ELTCSMRY
00609  SETUP-BP-POINTER-FOR-SUBTOPIC.                                   ELTCSMRY
00610      SET BP-SUBTOPIC-SELECTED TO TRUE.                            ELTCSMRY
00611      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELTCSMRY
00612      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCSMRY
00613           ADDRESS OF CSPT-POINTER-LIST.                           ELTCSMRY
00614      IF CIA-RC-PTR-NULL                                           ELTCSMRY
00615          PERFORM ACQUIRE-BENEFIT-PROVISION-POIN.                  ELTCSMRY
00616      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELTCSMRY
00617      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCSMRY
00618           ADDRESS OF CSPT-POINTER-LIST.                           ELTCSMRY
00619      IF FIRST-TIME                                                ELTCSMRY
00620          PERFORM INITIALIZE-ALL-SUBTOPIC-PROCES.                  ELTCSMRY
00621 ***********************************************************       ELTCSMRY
00622 ** NOTE => THE REQUEST LIST POINTER WILL BE ESTABLISHED TO*       ELTCSMRY
00623 **         DISTINGUISH WHAT SUBTOPIC POINTERS TO PROCESS. *       ELTCSMRY
00624 ***********************************************************       ELTCSMRY
00625      IF IHS OR ALL-FOUND                                          ELTCSMRY
00626          PERFORM DETERMINE-IF-MEDICARE-OR-NON-M.                  ELTCSMRY
00627      IF IPS OR ALL-FOUND                                          ELTCSMRY
00628         CALL 'ELUADDRS' USING WS-IPS-PHYSICIAN-SERVICES           ELTCSMRY
00629                               CSPT-IPS-REQ-LIST-PTR.              ELTCSMRY
00630      IF OPS OR ALL-FOUND                                          ELTCSMRY
00631         CALL 'ELUADDRS' USING WS-OPS-OUTPATIENT-SERVICES          ELTCSMRY
00632                               CSPT-OPS-REQ-LIST-PTR.              ELTCSMRY
00633      IF OBS OR ALL-FOUND                                          ELTCSMRY
00634         CALL 'ELUADDRS' USING WS-OBS-OBSTERILIZATION              ELTCSMRY
00635                               CSPT-OBS-REQ-LIST-PTR.              ELTCSMRY
00636      IF PSY OR ALL-FOUND                                          ELTCSMRY
00637         CALL 'ELUADDRS' USING WS-PSY-PSYCHIATRIC                  ELTCSMRY
00638                               CSPT-PSY-REQ-LIST-PTR.              ELTCSMRY
00639      EJECT                                                        ELTCSMRY
00640                                                                   ELTCSMRY
00641                                                                   ELTCSMRY
00642 ************************************************************      ELTCSMRY
00643 *                                                          *      ELTCSMRY
00644 *        ACQUIRE BENEFIT PROVISION POINTER AREA            *      ELTCSMRY
00645 *                                                          *      ELTCSMRY
00646 ************************************************************      ELTCSMRY
00647  ACQUIRE-BENEFIT-PROVISION-POIN.                                  ELTCSMRY
00648      SET CIA-ELSCSPTC-DDN TO TRUE.                                ELTCSMRY
00649      MOVE LENGTH OF CSPT-POINTER-LIST TO CIA-AREA-LEN.            ELTCSMRY
00650      PERFORM ALLOCATE-STORAGE-AREA.                               ELTCSMRY
00651      SET FIRST-TIME          TO TRUE.                             ELTCSMRY
00652                                                                   ELTCSMRY
00653                                                                   ELTCSMRY
00654 ************************************************************      ELTCSMRY
00655 *                                                          *      ELTCSMRY
00656 *        ALLOCATE STORAGE AREA                             *      ELTCSMRY
00657 *                                                          *      ELTCSMRY
00658 ************************************************************      ELTCSMRY
00659  ALLOCATE-STORAGE-AREA.                                           ELTCSMRY
00660      SET CIA-STG-GETMAIN TO TRUE.                                 ELTCSMRY
00661      PERFORM CALL-STORAGE-MANAGER.                                ELTCSMRY
00662      EJECT                                                        ELTCSMRY
00663                                                                   ELTCSMRY
00664                                                                   ELTCSMRY
00665 ************************************************************      ELTCSMRY
00666 *                                                          *      ELTCSMRY
00667 *        INITIALIZE ALL SUBTOPIC PROCESS SWITCHES          *      ELTCSMRY
00668 *                                                          *      ELTCSMRY
00669 ************************************************************      ELTCSMRY
00670  INITIALIZE-ALL-SUBTOPIC-PROCES.                                  ELTCSMRY
00671      INITIALIZE CSPT-SUBTOPIC-PROCESS-SW (1)                      ELTCSMRY
00672                 CSPT-SUBTOPIC-PROCESS-SW (2)                      ELTCSMRY
00673                 CSPT-SUBTOPIC-PROCESS-SW (3)                      ELTCSMRY
00674                 CSPT-SUBTOPIC-PROCESS-SW (4)                      ELTCSMRY
00675                 CSPT-SUBTOPIC-PROCESS-SW (5)                      ELTCSMRY
00676                 WS-FIRST-TIME-SW.                                 ELTCSMRY
00677      EJECT                                                        ELTCSMRY
00678                                                                   ELTCSMRY
00679                                                                   ELTCSMRY
00680 ************************************************************      ELTCSMRY
00681 *                                                          *      ELTCSMRY
00682 *        DETERMINE IF MEDICARE OR NON MEDICARE             *      ELTCSMRY
00683 *                                                          *      ELTCSMRY
00684 ************************************************************      ELTCSMRY
00685  DETERMINE-IF-MEDICARE-OR-NON-M.                                  ELTCSMRY
00686      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTCSMRY
00687      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCSMRY
00688          ADDRESS OF CONTRACT-RECORD.                              ELTCSMRY
00689      IF    GCT-FAM-REL-LVL = PC-FAM-REL-LVL-MEDIC                 ELTCSMRY
00690         OR GCG-FAM-REL-LVL = PC-FAM-REL-LVL-MEDIC                 ELTCSMRY
00691         CALL 'ELUADDRS' USING WS-IHM-MED-HOSPITAL-SERVICES        ELTCSMRY
00692                               CSPT-IHS-REQ-LIST-PTR               ELTCSMRY
00693      ELSE                                                         ELTCSMRY
00694         CALL 'ELUADDRS' USING WS-IHS-HOSPITAL-SERVICES            ELTCSMRY
00695                               CSPT-IHS-REQ-LIST-PTR.              ELTCSMRY
00696      EJECT                                                        ELTCSMRY
00697                                                                   ELTCSMRY
00698                                                                   ELTCSMRY
00699                                                                   ELTCSMRY
00700 ************************************************************      ELTCSMRY
00701 *                                                          *      ELTCSMRY
00702 *        CALL COVERAGE PROGRAM                             *      ELTCSMRY
00703 *                                                          *      ELTCSMRY
00704 ************************************************************      ELTCSMRY
00705  CALL-COVERAGE-PROGRAM.                                           ELTCSMRY
00706      MOVE +5                  TO CSPT-TBL-CNT.                    ELTCSMRY
00707      EXEC CICS LINK                                               ELTCSMRY
00708                PROGRAM ('ELUCSCOV')                               ELTCSMRY
00709                COMMAREA (DFHCOMMAREA)                             ELTCSMRY
00710                END-EXEC.                                          ELTCSMRY
00711                                                                   ELTCSMRY
00712                                                                   ELTCSMRY
00713 ************************************************************      ELTCSMRY
00714 *                                                          *      ELTCSMRY
00715 *        CALL REQUESTED PROGRAM                            *      ELTCSMRY
00716 *                                                          *      ELTCSMRY
00717 ************************************************************      ELTCSMRY
00718  CALL-REQUESTED-PROGRAM.                                          ELTCSMRY
00719      MOVE CS-MODULE-NAME (CS-IDX) TO WS-MODULE-NAME.              ELTCSMRY
00720      EXEC CICS LINK                                               ELTCSMRY
00721                PROGRAM (WS-MODULE-NAME)                           ELTCSMRY
00722                COMMAREA (DFHCOMMAREA)                             ELTCSMRY
00723                END-EXEC.                                          ELTCSMRY
00724 ************************************************                  ELTCSMRY
00725 ** THIS LOGIC WILL DISPLAY THE DISCLAIMER      *                  ELTCSMRY
00726 ** MESSAGE THAT WILL END EACH SUBTOPIC'S TEXT. *                  ELTCSMRY
00727 ************************************************                  ELTCSMRY
00728      IF CS-KEYWORD (CS-IDX) = 'GCI'                               ELTCSMRY
00729          PERFORM DISPLAY-GENERAL-CONTRACT-MESSA                   ELTCSMRY
00730      ELSE                                                         ELTCSMRY
00731          PERFORM DISPLAY-SUBTOPIC-MESSAGE.                        ELTCSMRY
00732      EJECT                                                        ELTCSMRY
00733                                                                   ELTCSMRY
00734                                                                   ELTCSMRY
00735 ************************************************************      ELTCSMRY
00736 *                                                          *      ELTCSMRY
00737 *        DISPLAY GENERAL CONTRACT MESSAGE                  *      ELTCSMRY
00738 *                                                          *      ELTCSMRY
00739 ************************************************************      ELTCSMRY
00740  DISPLAY-GENERAL-CONTRACT-MESSA.                                  ELTCSMRY
00741      ADD +1                     TO COF-NBR-DTL-LINES.             ELTCSMRY
00742      MOVE WS-DASH-LINE          TO COF-DTL-LINE                   ELTCSMRY
00743          (COF-NBR-DTL-LINES).                                     ELTCSMRY
00744      ADD +1                     TO COF-NBR-DTL-LINES.             ELTCSMRY
00745      MOVE WS-DISCLAIMER-1       TO COF-DTL-LINE                   ELTCSMRY
00746          (COF-NBR-DTL-LINES).                                     ELTCSMRY
00747      ADD +1                     TO COF-NBR-DTL-LINES.             ELTCSMRY
00748      MOVE WS-DISCLAIMER-2       TO COF-DTL-LINE                   ELTCSMRY
00749          (COF-NBR-DTL-LINES).                                     ELTCSMRY
00750      PERFORM LINK-TO-OUTPUT-INTERFACE.                            ELTCSMRY
00751      EJECT                                                        ELTCSMRY
00752                                                                   ELTCSMRY
00753                                                                   ELTCSMRY
00754 ************************************************************      ELTCSMRY
00755 *                                                          *      ELTCSMRY
00756 *        DISPLAY SUBTOPIC MESSAGE                          *      ELTCSMRY
00757 *                                                          *      ELTCSMRY
00758 ************************************************************      ELTCSMRY
00759  DISPLAY-SUBTOPIC-MESSAGE.                                        ELTCSMRY
00760      ADD +1                     TO COF-NBR-DTL-LINES.             ELTCSMRY
00761      MOVE WS-DASH-LINE          TO COF-DTL-LINE                   ELTCSMRY
00762          (COF-NBR-DTL-LINES).                                     ELTCSMRY
00763      ADD +1                     TO COF-NBR-DTL-LINES.             ELTCSMRY
00764      MOVE WS-DISCLAIMER-3       TO COF-DTL-LINE                   ELTCSMRY
00765          (COF-NBR-DTL-LINES).                                     ELTCSMRY
00766      ADD +1                     TO COF-NBR-DTL-LINES.             ELTCSMRY
00767      MOVE WS-DISCLAIMER-4       TO COF-DTL-LINE                   ELTCSMRY
00768          (COF-NBR-DTL-LINES).                                     ELTCSMRY
00769      PERFORM LINK-TO-OUTPUT-INTERFACE.                            ELTCSMRY
00770      EJECT                                                        ELTCSMRY
00771                                                                   ELTCSMRY
00772                                                                   ELTCSMRY
00773 ************************************************************      ELTCSMRY
00774 *                                                          *      ELTCSMRY
00775 *        TERMINATE OUTPUT DISPLAY                          *      ELTCSMRY
00776 *                                                          *      ELTCSMRY
00777 ************************************************************      ELTCSMRY
00778  TERMINATE-OUTPUT-DISPLAY.                                        ELTCSMRY
00779 *******************************                                   ELTCSMRY
00780 *** THIS LINK IS TO END THE  **                                   ELTCSMRY
00781 *** PAGE FOR ALL THE TEXT    **                                   ELTCSMRY
00782 *** DISPLAYED.  @@@ REB @@@  **                                   ELTCSMRY
00783 *******************************                                   ELTCSMRY
00784      MOVE +0     TO COF-NBR-HDR-LINES                             ELTCSMRY
00785                     COF-NBR-DTL-LINES.                            ELTCSMRY
00786      SET COF-END TO TRUE.                                         ELTCSMRY
00787      PERFORM LINK-TO-OUTPUT-INTERFACE.                            ELTCSMRY
00788                                                                   ELTCSMRY
00789                                                                   ELTCSMRY
00790 ************************************************************      ELTCSMRY
00791 *                                                          *      ELTCSMRY
00792 *        LINK TO OUTPUT INTERFACE                          *      ELTCSMRY
00793 *                                                          *      ELTCSMRY
00794 ************************************************************      ELTCSMRY
00795  LINK-TO-OUTPUT-INTERFACE.                                        ELTCSMRY
00796      EXEC CICS LINK                                               ELTCSMRY
00797                PROGRAM ('ELUOUTPT')                               ELTCSMRY
00798                COMMAREA (DFHCOMMAREA)                             ELTCSMRY
00799                END-EXEC.                                          ELTCSMRY
00800      EJECT                                                        ELTCSMRY
00801                                                                   ELTCSMRY
